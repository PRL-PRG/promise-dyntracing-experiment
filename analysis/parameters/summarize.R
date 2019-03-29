#!/usr/bin/env Rscript

## WARN - no good way in R to find path of current script
##        perhaps the only good solution is to create a
##        package for the analysis ?

##force_order_count should be changed to fore_order_call_count
##also add force_order_count field to the main function table.

options(error = quote({dump.frames(to.file=FALSE); q();}))

## this ensures that all the columns of the tibble are printed
## irrespective of terminal width
options(tibble.width = Inf)

suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(optparse))
suppressPackageStartupMessages(library(magrittr))
suppressPackageStartupMessages(library(stringr))
suppressPackageStartupMessages(library(fs))
suppressPackageStartupMessages(library(promisedyntracer))
suppressPackageStartupMessages(library(purrr))
suppressPackageStartupMessages(library(tidyr))
suppressPackageStartupMessages(library(tibble))

info <- function(...) cat((paste0(...)))

summarize_outliers <- function(df, grouping_column,
                               count_column, relative_count_column,
                               outlier_value) {

    grouping_column <- enquo(grouping_column)
    count_column <- enquo(count_column)
    relative_count_column <- enquo(relative_count_column)

    outlier_row <-
        df %>%
        filter(!! grouping_column > outlier_value) %>%
        mutate(!!count_column := as.numeric(!!count_column)) %>%
        summarize(!!grouping_column := str_c(">", toString(outlier_value), sep = " "),
                  !!count_column := sum(!!count_column),
                  !!relative_count_column := sum(!!relative_count_column))

    df <-
        df %>%
        mutate(!! grouping_column := as.character(!! grouping_column)) %>%
        bind_rows(outlier_row)

    df
}


events <- function(analyses) {

    event_counts <-
        analyses$event_counts %>%
        group_by(event) %>%
        summarize(count = sum(as.double(count)))

    list(event_counts = event_counts)
}


objects <- function(analyses) {
    ## object type count is already summarized by the tracer.
    ## we only have to emit the same file again for summarization.
    object_count_by_type <-
        analyses$object_counts %>%
        group_by(type) %>%
        summarize(count = sum(count)) %>%
        ungroup() %>%
        filter(!(type %in% c("Null", "..."))) %>%
        mutate(relative_count = count / sum(count))

    main_types <- c("Promise", "Environment", "Character", "Logical",
                    "Integer", "Double", "Raw", "Complex", "List", "Closure")

    internal_type <- c("Pairlist", "Function Call", "Char", "...", "Bytecode")

    main_object_count_by_type <-
        object_count_by_type %>%
        filter(!(type %in% internal_type)) %>%
        filter(type %in% main_types) %>%
        mutate(group_type = type) %>%
        mutate(type = "Main")

    other_object_count_by_type <-
        object_count_by_type %>%
        filter(!(type %in% internal_type)) %>%
        filter(!(type %in% main_types)) %>%
        mutate(group_type = "Other")

    grouped_object_count_by_type <-
        bind_rows(main_object_count_by_type,
                  other_object_count_by_type) %>%
        mutate(relative_count = count / sum(count))

    list(object_count_by_type = object_count_by_type,
         grouped_object_count_by_type = grouped_object_count_by_type)
}

## TODO - one of the summarizations has computed relative counts
##        incorrectly. Values are repeated and their relative count
##        should be more than 100 but it computes it as 100
functions <- function(analyses) {

    closure_count_by_call_count <-
        analyses$function_call_summary %>%
        filter(function_type == "Closure") %>%
        group_by(function_id) %>%
        summarize(call_count = sum(call_count)) %>%
        ungroup() %>%
        group_by(call_count) %>%
        summarize(closure_count = length(unique(function_id))) %>%
        mutate(relative_closure_count = closure_count / sum(closure_count)) %>%
        ungroup() %>%
        summarize_outliers(call_count, closure_count, relative_closure_count, 10)

    function_count_by_type <-
        analyses$function_call_summary %>%
        group_by(function_type) %>%
        summarize(function_count = length(unique(function_id))) %>%
        ungroup() %>%
        mutate(relative_function_count = function_count / sum(function_count))

    primitive_function_table <-
        analyses$function_call_summary %>%
        filter(function_type != "Closure") %>%
        group_by(function_type, function_id,
                 S3_method, S4_method,
                 jumped, return_value_type) %>%
        summarize(call_count = sum(call_count)) %>%
        ungroup()

    total_function_count <-
        function_count_by_type %>%
        pull(function_count) %>%
        sum()

    total_closure_count <-
        function_count_by_type %>%
        filter(function_type == "Closure") %>%
        pull(function_count)

    total_special_count <-
        function_count_by_type %>%
        filter(function_type == "Special") %>%
        pull(function_count)

    total_builtin_count <-
        function_count_by_type %>%
        filter(function_type == "Builtin") %>%
        pull(function_count)

    function_call_count_by_type <-
        analyses$function_call_summary %>%
        group_by(function_type) %>%
        summarize(call_count = sum(call_count)) %>%
        ungroup() %>%
        mutate(relative_call_count = call_count / sum(call_count))

    function_call_count_by_return_value_type <-
        analyses$function_call_summary %>%
        filter(!jumped) %>%
        group_by(function_type, return_value_type) %>%
        summarize(call_count = sum(call_count)) %>%
        ungroup() %>%
        mutate(relative_call_count = call_count / sum(call_count))

    closure_call_count_by_return_value_type <-
        function_call_count_by_return_value_type %>%
        filter(function_type == "Closure") %>%
        mutate(relative_call_count = call_count / sum(call_count))

    function_count_by_return_value_type <-
        analyses$function_call_summary %>%
        filter(!jumped) %>%
        group_by(function_type, return_value_type) %>%
        summarize(function_count = length(unique(function_id))) %>%
        ungroup() %>%
        left_join(
            function_count_by_type %>%
            select(function_type, total_function_count = function_count),
            by = "function_type") %>%
        mutate(relative_function_count = function_count / total_function_count)

    closure_formal_parameter_count_table <-
        analyses$closure_parameter_count %>%
        group_by(function_id) %>%
        summarize(formal_parameter_count = first(formal_parameter_count)) %>%
        ungroup()

    closure_count_by_formal_parameter_count <-
        analyses$closure_parameter_count %>%
        group_by(formal_parameter_count) %>%
        summarize(closure_count = length(unique(function_id))) %>%
        ungroup() %>%
        mutate(relative_closure_count = closure_count / sum(closure_count)) %>%
        summarize_outliers(formal_parameter_count, closure_count,
                           relative_closure_count, 5)

    function_call_count_by_type_and_jump <-
        analyses$function_call_summary %>%
        group_by(function_type, jumped) %>%
        summarize(call_count = sum(call_count)) %>%
        ungroup() %>%
        mutate(relative_call_count = call_count / sum(call_count))

    function_call_count_by_type_and_jump <-
        analyses$function_call_summary %>%
        group_by(function_type, jumped) %>%
        summarize(call_count = sum(call_count)) %>%
        ungroup() %>%
        mutate(relative_call_count = call_count / sum(call_count))

    function_call_count_by_type_and_id <-
        analyses$function_call_summary %>%
        group_by(function_id) %>%
        summarize(function_type = first(function_type),
                  call_count = sum(as.double(call_count))) %>%
        ungroup() %>%
        mutate(relative_call_count = call_count / sum(call_count))

    compute_method_type <- function(S3_method, S4_method) {
        if_else(!S3_method & !S4_method,
                "Ordinary",
                if_else(S3_method & !S4_method,
                        "S3",
                        if_else(!S3_method & S4_method,
                                "S4",
                                "S3 & S4")))
    }

    function_method_type <-
        analyses$function_call_summary %>%
        group_by(function_type, function_id, S3_method, S4_method) %>%
        summarize(call_count = sum(call_count)) %>%
        ungroup() %>%
        mutate(method_type = compute_method_type(S3_method, S4_method)) %>%
        select(function_type, function_id, method_type, call_count)

    function_call_count_by_method_type <-
        function_method_type %>%
        group_by(function_type, method_type) %>%
        summarize(call_count = sum(call_count)) %>%
        ungroup() %>%
        mutate(relative_call_count = call_count / sum(call_count))

    function_count_by_method_type <-
        function_method_type %>%
        group_by(function_type, method_type) %>%
        summarize(function_count = length(unique(function_id))) %>%
        ungroup() %>%
        mutate(relative_function_count = function_count / total_function_count)

    split_pos_seq <- function(pos_seq) {
        map(str_split(str_sub(pos_seq, 2, -2), " "),
            function(chr_lst) if(length(chr_lst) == 1 && chr_lst == "") c() else chr_lst)
    }

    pos_seq_eq <- function(pos_seq_1, pos_seq_2) {
        l1 <- length(pos_seq_1)
        l2 <- length(pos_seq_2)

        if(l1 != l2) {
            FALSE
        }
        else if(l1 == 0) {
            TRUE
        }
        else {
            all(pos_seq_1 == pos_seq_2)
        }
    }

    closure_force_order_count <-
        analyses$closure_force_order_count %>%
        group_by(wrapper, function_id, force_order, missing_arguments) %>%
        summarize(formal_parameter_count = first(formal_parameter_count),
                  call_count = sum(call_count)) %>%
        ungroup() %>%
        mutate(force_order = split_pos_seq(force_order),
               missing_arguments = split_pos_seq(missing_arguments)) %>%
        group_by(wrapper, function_id) %>%
        do({
            missing_data <-
                .data %>%
                filter(lengths(missing_arguments) != 0)

            missing_force_orders <- missing_data$force_order
            all_missing_arguments <- missing_data$missing_arguments

            non_missing_data <-
                .data %>%
                filter(lengths(missing_arguments) == 0)

            non_missing_force_orders <- unique(non_missing_data$force_order)

            compatible_force_orders <-
                map2(missing_force_orders,
                     all_missing_arguments,
                     function(missing_force_order, missing_arguments) {
                         for (non_missing_force_order in non_missing_force_orders) {
                             compatible_force_order <-
                                 non_missing_force_order[! non_missing_force_order %in% missing_arguments]
                             if(pos_seq_eq(compatible_force_order, missing_force_order)) {
                                 return(non_missing_force_order)
                             }
                         }
                         missing_force_order
                     })

            non_missing_data <-
                non_missing_data %>%
                mutate(compatible_force_order = force_order)

            missing_data <-
                missing_data %>%
                mutate(compatible_force_order = compatible_force_orders)

            bind_rows(non_missing_data,
                      missing_data)
        }) %>%
        ungroup()

    pos_seq_to_string <- function(pos_seq_list) {
        map_chr(pos_seq_list,
            function(pos_seq) {
                str_c("(", str_c(pos_seq, collapse = " "), ")")
            })
    }

    closure_force_order_count <-
        closure_force_order_count %>%
        mutate(force_order = pos_seq_to_string(force_order),
               compatible_force_order = pos_seq_to_string(compatible_force_order),
               missing_arguments = pos_seq_to_string(missing_arguments))

    zero_parameter_closure_force_order_count <-
        closure_force_order_count %>%
        filter(formal_parameter_count == 0) %>%
        group_by(wrapper) %>%
        do({
            r <- nrow(.data)
            tibble(closure_count = c(r, r),
                   force_order_type = c("Original", "Compatible"))
        }) %>%
        ungroup() %>%
        mutate(force_order_count = "0",
               relative_closure_count = closure_count / total_closure_count)

    non_zero_parameter_multicall_closure_force_order_count <-
        closure_force_order_count %>%
        filter(formal_parameter_count != 0 & call_count > 1) %>%
        group_by(wrapper, function_id) %>%
        summarize(
            force_order = to_sequence(unique(force_order)),
            force_order_count = length(unique(force_order)),
            compatible_force_order = to_sequence(),
            compatible_force_order_count = length(unique(compatible_force_order)),
            formal_parameter_count = first(formal_parameter_count)) %>%
        ungroup()

    closure_count_distribution_by_wrapper_and_original_force_order <-
        non_zero_parameter_multicall_closure_force_order_count %>%
        group_by(wrapper, force_order_count) %>%
        summarize(closure_count = n()) %>%
        ungroup() %>%
        mutate(relative_closure_count = closure_count / total_closure_count,
               force_order_type = "Original")

    closure_count_distribution_by_wrapper_and_compatible_force_order <-
        non_zero_parameter_multicall_closure_force_order_count %>%
        group_by(wrapper, compatible_force_order_count) %>%
        summarize(closure_count = n()) %>%
        ungroup() %>%
        mutate(relative_closure_count = closure_count / total_closure_count,
               force_order_type = "Compatible") %>%
        select(wrapper, force_order_type,
               force_order_count = compatible_force_order_count,
               closure_count, relative_closure_count)

    closure_count_distribution_by_wrapper_and_force_order <-
        bind_rows(closure_count_distribution_by_wrapper_and_original_force_order,
                  closure_count_distribution_by_wrapper_and_compatible_force_order)

    closure_count_distribution_by_wrapper_and_force_order_outliers <-
        closure_count_distribution_by_wrapper_and_force_order %>%
        filter(force_order_count > 5) %>%
        group_by(wrapper, force_order_type) %>%
        summarize(force_order_count = " > 5",
                  closure_count = sum(closure_count),
                  relative_closure_count = sum(relative_closure_count)) %>%
        ungroup()

    closure_count_by_wrapper_and_force_order <-
        closure_count_distribution_by_wrapper_and_force_order %>%
        mutate(force_order_count = as.character(force_order_count)) %>%
        bind_rows(closure_count_distribution_by_wrapper_and_force_order_outliers,
                  zero_parameter_closure_force_order_count) %>%
        mutate(wrapper = if_else(wrapper == 0, "Non Wrapper", "Wrapper"))

    list(closure_count_by_call_count = closure_count_by_call_count,
         function_count_by_type = function_count_by_type,
         primitive_function_table = primitive_function_table,
         function_call_count_by_type = function_call_count_by_type,
         function_call_count_by_return_value_type = function_call_count_by_return_value_type,
         closure_call_count_by_return_value_type = closure_call_count_by_return_value_type,
         function_count_by_return_value_type = function_count_by_return_value_type,
         closure_formal_parameter_count_table = closure_formal_parameter_count_table,
         closure_count_by_formal_parameter_count = closure_count_by_formal_parameter_count,
         function_call_count_by_type_and_jump = function_call_count_by_type_and_jump,
         function_call_count_by_type_and_id = function_call_count_by_type_and_id,
         function_call_count_by_method_type = function_call_count_by_method_type,
         function_count_by_method_type = function_count_by_method_type,
         closure_force_order_count = closure_force_order_count,
         non_zero_parameter_multicall_closure_force_order_count = non_zero_parameter_multicall_closure_force_order_count,
         closure_count_by_wrapper_and_force_order = closure_count_by_wrapper_and_force_order)
}

arguments <- function(analyses) {

    summarize_event_counts <- function(df, group_column) {
        group_column <- enquo(group_column)

        df %>%
            mutate(argument_count = as.numeric(argument_count)) %>%
            group_by(!!group_column) %>%
            summarize(argument_count = sum(argument_count)) %>%
            ungroup() %>%
            mutate(relative_argument_count = argument_count / sum(argument_count))
    }

    argument_count_by_type <-
        analyses$argument_count_by_type %>%
        summarize_event_counts(argument_type) %>%
        arrange(desc(relative_argument_count))

    non_missing_argument_count_by_dot_dot_dot <-
        analyses$non_missing_argument_count_by_dot_dot_dot %>%
        summarize_event_counts(argument_category)

    promise_argument_count_by_nature <-
        analyses$promise_argument_count_by_nature %>%
        summarize_event_counts(argument_nature)

    promise_argument_count_by_sharing <-
        analyses$promise_argument_count_by_sharing %>%
        mutate(promise_count = as.numeric(promise_count)) %>%
        group_by(sharing_count) %>%
        summarize(promise_count = sum(promise_count)) %>%
        ungroup() %>%
        mutate(relative_promise_count = promise_count / sum(promise_count)) %>%
        summarize_outliers(sharing_count, promise_count, relative_promise_count, 3)

    ## promise_argument_count_by_expression_type <-
    ##     analyses$promise_argument_count_by_expression_type %>%
    ##     summarize_event_counts(expression_type)

    ## promise_argument_count_by_value_type <-
    ##     analyses$promise_argument_count_by_value_type %>%
    ##     summarize_event_counts(value_type)

    promise_argument_count_by_force_type <-
        analyses$promise_argument_count_by_force_type %>%
        summarize_event_counts(force_type)

    ## promise_argument_count_by_direct_lookup_count <-
    ##     analyses$promise_argument_count_by_direct_lookup_count %>%
    ##     summarize_event_counts(direct_lookup_count) %>%
    ##     summarize_outliers(direct_lookup_count,
    ##                        argument_count,
    ##                        relative_argument_count,
    ##                        5)

    ## promise_argument_count_by_direct_and_indirect_lookup_count <-
    ##     analyses$promise_argument_count_by_direct_and_indirect_lookup_count %>%
    ##     summarize_event_counts(direct_and_indirect_lookup_count) %>%
    ##     summarize_outliers(direct_and_indirect_lookup_count,
    ##                        argument_count,
    ##                        relative_argument_count,
    ##                        5)

    ## promise_argument_count_by_direct_metaprogram_count <-
    ##     analyses$promise_argument_count_by_direct_metaprogram_count %>%
    ##     summarize_event_counts(direct_metaprogram_count) %>%
    ##     summarize_outliers(direct_metaprogram_count,
    ##                        argument_count,
    ##                        relative_argument_count,
    ##                        5)

    ## promise_argument_count_by_direct_and_indirect_metaprogram_count <-
    ##     analyses$promise_argument_count_by_direct_and_indirect_metaprogram_count %>%
    ##     summarize_event_counts(direct_and_indirect_metaprogram_count) %>%
    ##     summarize_outliers(direct_and_indirect_metaprogram_count,
    ##                        argument_count,
    ##                        relative_argument_count,
    ##                        5)

    ## promise_argument_count_by_dispatch_type <-
    ##     analyses$promise_argument_count_by_dispatch_type %>%
    ##     summarize_event_counts(dispatch_type)

    promise_argument_forced_by_promise_argument <-
        analyses$promise_argument_forced_by_promise_argument %>%
        group_by(function_id,
                 formal_parameter_position,
                 actual_argument_position,
                 default,
                 forcing_actual_argument_position) %>%
        summarize(count = n())

    promise_argument_returning_non_locally <-
        analyses$promise_argument_returning_non_locally %>%
        group_by(function_id,
                 formal_parameter_position,
                 actual_argument_position,
                 default,
                 expression_type,
                 value_type) %>%
        summarize(count = n())

    list(argument_count_by_type = argument_count_by_type,
         non_missing_argument_count_by_dot_dot_dot = non_missing_argument_count_by_dot_dot_dot,
         promise_argument_count_by_nature = promise_argument_count_by_nature,
         promise_argument_count_by_sharing = promise_argument_count_by_sharing,
         ## promise_argument_count_by_expression_type = promise_argument_count_by_expression_type,
         ## promise_argument_count_by_value_type = promise_argument_count_by_value_type,
         promise_argument_count_by_force_type = promise_argument_count_by_force_type,
         ## promise_argument_count_by_direct_lookup_count = promise_argument_count_by_direct_lookup_count,
         ## promise_argument_count_by_direct_and_indirect_lookup_count = promise_argument_count_by_direct_and_indirect_lookup_count,
         ## promise_argument_count_by_direct_metaprogram_count = promise_argument_count_by_direct_metaprogram_count,
         ## promise_argument_count_by_direct_and_indirect_metaprogram_count = promise_argument_count_by_direct_and_indirect_metaprogram_count,
         ## promise_argument_count_by_dispatch_type = promise_argument_count_by_dispatch_type,
         promise_argument_forced_by_promise_argument = promise_argument_forced_by_promise_argument,
         promise_argument_returning_non_locally = promise_argument_returning_non_locally)
}


parameters <- function(analyses) {

    classify_parameter <- function(argument_use_count, count) {
        if_else(argument_use_count == count, "Always",
        if_else(argument_use_count == 0, "Never",
                "Sometimes"))
    }

    total_argument_count <-
        analyses$formal_parameter_usage_counts %>%
        pull(argument_count) %>%
        as.double() %>%
        sum()

    looked_up_and_metaprogrammed_argument_count <-
        analyses$formal_parameter_usage_counts %>%
        pull(both) %>%
        as.double() %>%
        sum()

    looked_up_argument_count <-
        analyses$formal_parameter_usage_counts %>%
        pull(lookup) %>%
        as.double() %>%
        sum() %>%
        `-`(looked_up_and_metaprogrammed_argument_count)

    metaprogrammed_argument_count <-
        analyses$formal_parameter_usage_counts %>%
        pull(metaprogram) %>%
        as.double() %>%
        sum() %>%
        `-`(looked_up_and_metaprogrammed_argument_count)

    none_argument_count <- (total_argument_count -
                            looked_up_argument_count -
                            metaprogrammed_argument_count -
                            looked_up_and_metaprogrammed_argument_count)

    argument_count_by_usage <-
        tibble(argument_use = c("Lookup", "Metaprogram", "Both", "None"),
               argument_count = c(looked_up_argument_count,
                                  metaprogrammed_argument_count,
                                  looked_up_and_metaprogrammed_argument_count,
                                  none_argument_count)) %>%
        mutate(relative_argument_count = argument_count / total_argument_count)

    formal_parameter_usage_class <-
        analyses$formal_parameter_usage_counts %>%
        group_by(function_id, formal_parameter_position) %>%
        summarize(lookup = sum(lookup),
                  metaprogram = sum(metaprogram),
                  either = sum(either),
                  argument_count = sum(argument_count),
                  call_count = sum(call_count)) %>%
        ungroup() %>%
        mutate(lookup_class = classify_parameter(lookup, argument_count),
               metaprogram_class = classify_parameter(metaprogram, argument_count),
               either_class = classify_parameter(either, argument_count)) %>%
        mutate(parameter_use = if_else((lookup == argument_count) & (metaprogram == 0), "Lookup",
                               if_else((lookup == 0) & (metaprogram == argument_count), "Metaprogram",
                               if_else((metaprogram == 0) & (lookup == 0), "Nothing", "Lookup & Metaprogram"))))

    total_formal_parameter_count <-
        formal_parameter_usage_class %>%
        nrow()

    formal_parameter_count_by_use <-
        formal_parameter_usage_class %>%
        group_by(parameter_use) %>%
        summarize(parameter_count = n()) %>%
        ungroup() %>%
        mutate(relative_parameter_count = parameter_count / sum(parameter_count))

    formal_parameter_count_by_usage_class <-
        formal_parameter_usage_class %>%
        select(function_id,
               formal_parameter_position,
               Lookup = lookup_class,
               Metaprogram = metaprogram_class,
               Either  = either_class) %>%
        gather(parameter_use,
               parameter_class,
               -function_id,
               -formal_parameter_position) %>%
        group_by(parameter_use, parameter_class) %>%
        summarize(parameter_count = n()) %>%
        ungroup() %>%
        mutate(relative_parameter_count = parameter_count / total_formal_parameter_count)

    classify_function <- function(parameter_use) {
        str_c(as.vector(sort(unique(parameter_use))), collapse = " & ")
    }

    total_closure_count <-
        analyses$function_call_summary %>%
        filter(function_type == "Closure") %>%
        pull(function_id) %>%
        unique() %>%
        length()

    closure_usage_class <-
        formal_parameter_usage_class %>%
        group_by(function_id) %>%
        summarize(Lookup = classify_function(lookup_class),
                  Metaprogram = classify_function(metaprogram_class),
                  Either = classify_function(either_class)) %>%
        ungroup()

    closure_count_distribution_by_usage_class <-
        closure_usage_class %>%
        gather(closure_use, closure_class, -function_id) %>%
        group_by(closure_use, closure_class) %>%
        summarize(closure_count = n()) %>%
        ungroup() %>%
        mutate(relative_closure_count = closure_count / total_closure_count)

    execution_times <-
        analyses$execution_times %>%
        group_by(function_id, formal_parameter_position, execution_time) %>%
        summarize(argument_count = sum(argument_count)) %>%
        ungroup() %>%
        left_join(formal_parameter_usage_class %>%
                  select(function_id, formal_parameter_position, lookup_class),
                  by = c("function_id", "formal_parameter_position")) %>%
        group_by(lookup_class, execution_time) %>%
        summarize(argument_count = sum(argument_count)) %>%
        mutate(relative_argument_count = argument_count / sum(argument_count)) %>%
        ungroup()

    list(argument_count_by_usage = argument_count_by_usage,
         formal_parameter_count_by_use = formal_parameter_count_by_use,
         formal_parameter_usage_class = formal_parameter_usage_class,
         formal_parameter_count_by_usage_class = formal_parameter_count_by_usage_class,
         closure_usage_class = closure_usage_class,
         closure_count_distribution_by_usage_class = closure_count_distribution_by_usage_class,
         execution_times = execution_times)
}


promises <- function(analyses) {

    summarize_event_counts <- function(df, group_column) {
        group_column <- enquo(group_column)

        df %>%
            mutate(promise_count = as.numeric(promise_count)) %>%
            group_by(!!group_column) %>%
            summarize(promise_count = sum(promise_count)) %>%
            ungroup() %>%
            mutate(relative_promise_count = promise_count / sum(promise_count))
    }

    promise_count_by_category <-
        analyses$promise_count_by_category %>%
        summarize_event_counts(promise_category)

    argument_promise_count_by_expression_type <-
        analyses$argument_promise_count_by_expression_type %>%
        summarize_event_counts(expression_type)

    ## non_argument_promise_count_by_expression_type <-
    ##     analyses$non_argument_promise_count_by_expression_type %>%
    ##     summarize_event_counts(expression_type)

    argument_promise_count_by_value_type <-
        analyses$argument_promise_count_by_value_type %>%
        summarize_event_counts(value_type)

    ## non_argument_promise_count_by_value_type <-
    ##     analyses$non_argument_promise_count_by_value_type %>%
    ##     summarize_event_counts(value_type)

    ## argument_promise_count_by_creation_scope <-
    ##     analyses$argument_promise_count_by_creation_scope %>%
    ##     summarize_event_counts(creation_scope)

    ## non_argument_promise_count_by_creation_scope <-
    ##     analyses$non_argument_promise_count_by_creation_scope %>%
    ##     summarize_event_counts(creation_scope)

    ## argument_promise_count_by_forcing_scope <-
    ##     analyses$argument_promise_count_by_forcing_scope %>%
    ##     summarize_event_counts(forcing_scope)

    ## non_argument_promise_count_by_forcing_scope <-
    ##     analyses$non_argument_promise_count_by_forcing_scope %>%
    ##     summarize_event_counts(forcing_scope)

    argument_promise_count_by_dispatch_type <-
        analyses$argument_promise_count_by_dispatch_type %>%
        summarize_event_counts(dispatch_type)

    argument_promise_count_by_call_depth <-
        analyses$argument_promise_count_by_call_depth %>%
        summarize_event_counts(call_depth) %>%
        summarize_outliers(call_depth,
                           promise_count,
                           relative_promise_count,
                           10)

    ## argument_promise_count_by_promise_depth <-
    ##     analyses$argument_promise_count_by_promise_depth %>%
    ##     summarize_event_counts(promise_depth) %>%
    ##     summarize_outliers(promise_depth,
    ##                        promise_count,
    ##                        relative_promise_count,
    ##                        10)

    ## argument_promise_count_by_nested_promise_depth <-
    ##     analyses$argument_promise_count_by_nested_promise_depth %>%
    ##     summarize_event_counts(nested_promise_depth) %>%
    ##     summarize_outliers(nested_promise_depth,
    ##                        promise_count,
    ##                        relative_promise_count,
    ##                        10)

    argument_promise_count_by_force_count <-
        analyses$argument_promise_count_by_force_count %>%
        summarize_event_counts(force_count)

    argument_promise_count_by_metaprogram_count <-
        analyses$argument_promise_count_by_metaprogram_count %>%
        summarize_event_counts(metaprogram_count) %>%
        summarize_outliers(metaprogram_count,
                           promise_count,
                           relative_promise_count,
                           10)

    argument_promise_count_by_value_lookup_count <-
        analyses$argument_promise_count_by_value_lookup_count %>%
        summarize_event_counts(value_lookup_count) %>%
        summarize_outliers(value_lookup_count,
                           promise_count,
                           relative_promise_count,
                           10)

    ## promise_count_by_value_assign_count <-
    ##     analyses$promise_count_by_value_assign_count %>%
    ##     summarize_event_counts(value_assign_count) %>%
    ##     summarize_outliers(value_assign_count,
    ##                        promise_count,
    ##                        relative_promise_count,
    ##                        10)

    argument_promise_count_by_expression_lookup_count <-
        analyses$argument_promise_count_by_expression_lookup_count %>%
        summarize_event_counts(expression_lookup_count) %>%
        summarize_outliers(expression_lookup_count,
                           promise_count,
                           relative_promise_count,
                           10)

    ## promise_count_by_expression_assign_count <-
    ##     analyses$promise_count_by_expression_assign_count %>%
    ##     summarize_event_counts(expression_assign_count) %>%
    ##     summarize_outliers(expression_assign_count,
    ##                        promise_count,
    ##                        relative_promise_count,
    ##                        10)

    ## promise_count_by_environment_lookup_count <-
    ##     analyses$promise_count_by_environment_lookup_count %>%
    ##     summarize_event_counts(environment_lookup_count) %>%
    ##     summarize_outliers(environment_lookup_count,
    ##                        promise_count,
    ##                        relative_promise_count,
    ##                        10)

    ## promise_count_by_environment_assign_count <-
    ##     analyses$promise_count_by_environment_assign_count %>%
    ##     summarize_event_counts(environment_assign_count) %>%
    ##     summarize_outliers(environment_assign_count,
    ##                        promise_count,
    ##                        relative_promise_count,
    ##                        10)

    ## promise_count_by_direct_self_scope_mutation <-
    ##     analyses$promise_count_by_direct_self_scope_mutation %>%
    ##     summarize_event_counts(direct_self_scope_mutation)

    ## promise_count_by_indirect_self_scope_mutation <-
    ##     analyses$promise_count_by_indirect_self_scope_mutation %>%
    ##     summarize_event_counts(indirect_self_scope_mutation)

    ## promise_count_by_direct_lexical_scope_mutation <-
    ##     analyses$promise_count_by_direct_lexical_scope_mutation %>%
    ##     summarize_event_counts(direct_lexical_scope_mutation)

    ## promise_count_by_indirect_lexical_scope_mutation <-
    ##     analyses$promise_count_by_indirect_lexical_scope_mutation %>%
    ##     summarize_event_counts(indirect_lexical_scope_mutation)

    ## promise_count_by_direct_non_lexical_scope_mutation <-
    ##     analyses$promise_count_by_direct_non_lexical_scope_mutation %>%
    ##     summarize_event_counts(direct_non_lexical_scope_mutation)

    ## promise_count_by_indirect_non_lexical_scope_mutation <-
    ##     analyses$promise_count_by_indirect_non_lexical_scope_mutation %>%
    ##     summarize_event_counts(indirect_non_lexical_scope_mutation)

    ## promise_count_by_direct_self_scope_observation <-
    ##     analyses$promise_count_by_direct_self_scope_observation %>%
    ##     summarize_event_counts(direct_self_scope_observation)

    ## promise_count_by_indirect_self_scope_observation <-
    ##     analyses$promise_count_by_indirect_self_scope_observation %>%
    ##     summarize_event_counts(indirect_self_scope_observation)

    ## promise_count_by_direct_lexical_scope_observation <-
    ##     analyses$promise_count_by_direct_lexical_scope_observation %>%
    ##     summarize_event_counts(direct_lexical_scope_observation)

    ## promise_count_by_indirect_lexical_scope_observation <-
    ##     analyses$promise_count_by_indirect_lexical_scope_observation %>%
    ##     summarize_event_counts(indirect_lexical_scope_observation)

    ## promise_count_by_direct_non_lexical_scope_observation <-
    ##     analyses$promise_count_by_direct_non_lexical_scope_observation %>%
    ##     summarize_event_counts(direct_non_lexical_scope_observation)

    ## promise_count_by_indirect_non_lexical_scope_observation <-
    ##     analyses$promise_count_by_indirect_non_lexical_scope_observation %>%
    ##     summarize_event_counts(indirect_non_lexical_scope_observation)

    argument_promise_use_distribution <-
        analyses$argument_promise_use_distribution %>%
        mutate(promise_count = as.numeric(promise_count)) %>%
        group_by(use) %>%
        summarize(promise_count = sum(promise_count)) %>%
        ungroup() %>%
        mutate(relative_promise_count = promise_count / sum(promise_count))

    argument_promise_action_distribution <-
        analyses$argument_promise_action_distribution %>%
        mutate(promise_count = as.numeric(promise_count)) %>%
        group_by(action) %>%
        summarize(promise_count = sum(promise_count)) %>%
        ungroup() %>%
        mutate(relative_promise_count = promise_count / sum(promise_count))

    list(promise_count_by_category = promise_count_by_category,
         argument_promise_count_by_expression_type = argument_promise_count_by_expression_type,
         ## non_argument_promise_count_by_expression_type = non_argument_promise_count_by_expression_type,
         argument_promise_count_by_value_type = argument_promise_count_by_value_type,
         ## non_argument_promise_count_by_value_type = non_argument_promise_count_by_value_type,
         ## argument_promise_count_by_creation_scope = argument_promise_count_by_creation_scope,
         ## non_argument_promise_count_by_creation_scope = non_argument_promise_count_by_creation_scope,
         ## argument_promise_count_by_forcing_scope = argument_promise_count_by_forcing_scope,
         ## non_argument_promise_count_by_forcing_scope = non_argument_promise_count_by_forcing_scope,
         argument_promise_count_by_dispatch_type = argument_promise_count_by_dispatch_type,
         argument_promise_count_by_call_depth = argument_promise_count_by_call_depth,
         ## argument_promise_count_by_promise_depth = argument_promise_count_by_promise_depth,
         ## argument_promise_count_by_nested_promise_depth = argument_promise_count_by_nested_promise_depth,
         argument_promise_count_by_force_count = argument_promise_count_by_force_count,
         argument_promise_count_by_metaprogram_count = argument_promise_count_by_metaprogram_count,
         argument_promise_count_by_value_lookup_count = argument_promise_count_by_value_lookup_count,
         ## promise_count_by_value_assign_count = promise_count_by_value_assign_count,
         argument_promise_count_by_expression_lookup_count = argument_promise_count_by_expression_lookup_count,
         ## promise_count_by_expression_assign_count = promise_count_by_expression_assign_count,
         ## promise_count_by_environment_lookup_count = promise_count_by_environment_lookup_count,
         ## promise_count_by_environment_assign_count = promise_count_by_environment_assign_count,
         ## promise_count_by_direct_self_scope_mutation = promise_count_by_direct_self_scope_mutation,
         ## promise_count_by_indirect_self_scope_mutation = promise_count_by_indirect_self_scope_mutation,
         ## promise_count_by_direct_lexical_scope_mutation = promise_count_by_direct_lexical_scope_mutation,
         ## promise_count_by_indirect_lexical_scope_mutation = promise_count_by_indirect_lexical_scope_mutation,
         ## promise_count_by_direct_non_lexical_scope_mutation = promise_count_by_direct_non_lexical_scope_mutation,
         ## promise_count_by_indirect_non_lexical_scope_mutation = promise_count_by_indirect_non_lexical_scope_mutation,
         ## promise_count_by_direct_self_scope_observation = promise_count_by_direct_self_scope_observation,
         ## promise_count_by_indirect_self_scope_observation = promise_count_by_indirect_self_scope_observation,
         ## promise_count_by_direct_lexical_scope_observation = promise_count_by_direct_lexical_scope_observation,
         ## promise_count_by_indirect_lexical_scope_observation = promise_count_by_indirect_lexical_scope_observation,
         ## promise_count_by_direct_non_lexical_scope_observation = promise_count_by_direct_non_lexical_scope_observation,
         ## promise_count_by_indirect_non_lexical_scope_observation = promise_count_by_indirect_non_lexical_scope_observation,
         argument_promise_use_distribution = argument_promise_use_distribution,
         argument_promise_action_distribution = argument_promise_action_distribution)

}


escaped_arguments <- function(analyses) {

    escaped_arguments <-
        analyses$escaped_arguments %>%
        group_by(function_id, formal_parameter_position, actual_argument_position,
                 argument_type, expression_type, value_type, return_value_type) %>%
        summarize(call_count = as.numeric(n())) %>%
        ungroup()

    escaped_argument_return_value_type <-
        analyses$escaped_arguments %>%
        group_by(package, script_type, script_name, call_id, function_id) %>%
        summarize(return_value_type = first(return_value_type)) %>%
        ungroup()

    escaped_argument_function_call_count_by_return_value_type <-
        escaped_argument_return_value_type %>%
        group_by(return_value_type) %>%
        summarize(call_count = n()) %>%
        ungroup() %>%
        mutate(relative_call_count = call_count / sum(call_count)) %>%
        add_column(category = "Escaped", .before = 1)

    escaped_argument_function_id <-
        escaped_arguments %>%
        pull(function_id) %>%
        unique()

    not_escaped_argument_function_call_count_by_return_value_type <-
        analyses$function_call_summary %>%
        filter(!jumped & function_type == "Closure") %>%
        filter(function_id %in% escaped_argument_function_id) %>%
        group_by(return_value_type) %>%
        summarize(call_count = sum(call_count)) %>%
        ungroup() %>%
        mutate(relative_call_count = call_count / sum(call_count)) %>%
        add_column(category = "Not-escaped", .before = 1)

    function_call_count_by_escape_category_and_return_value_type <-
        bind_rows(escaped_argument_function_call_count_by_return_value_type,
                  not_escaped_argument_function_call_count_by_return_value_type) %>%
        select(return_value_type, category, relative_call_count) %>%
        spread(category, relative_call_count, fill = 0) %>%
        gather(category, relative_call_count, -return_value_type)

    escaped_argument_function_count_by_return_value_type <-
        escaped_argument_return_value_type %>%
        group_by(return_value_type) %>%
        summarize(function_count = length(unique(function_id))) %>%
        ungroup() %>%
        mutate(relative_function_count = function_count / sum(function_count))

    categorize <- function(categories) {
        if (all(categories == "All")) {
            "All"
        }
        else if (all(categories == "Some")) {
            "Some"
        }
        else {
            "Both"
        }
    }

    escaped_argument_function_category <-
        analyses$escaped_arguments %>%
        group_by(package, script_type, script_name, call_id) %>%
        mutate(category = if_else(n() >= formal_parameter_count, "All", "Some")) %>%
        ungroup() %>%
        group_by(function_id) %>%
        summarize(category = categorize(category)) %>%
        ungroup()

    escaped_argument_function_count_by_category <-
        escaped_argument_function_category %>%
        group_by(category) %>%
        summarize(function_count = n()) %>%
        ungroup() %>%
        mutate(relative_function_count = function_count / sum(function_count))

    escaped_argument_count_by_nature <-
        analyses$escaped_arguments %>%
        group_by(default) %>%
        summarize(argument_count = n()) %>%
        ungroup() %>%
        mutate(relative_argument_count = argument_count / sum(argument_count)) %>%
        mutate(argument_nature = c("Non Default", "Default")[default + 1]) %>%
        select(argument_nature, argument_count, relative_argument_count)

    escaped_argument_count_by_dispatch_type <-
        analyses$escaped_arguments %>%
        mutate(dispatch_type = if_else(!S3_dispatch & !S4_dispatch, "Ordinary",
                                       if_else(S3_dispatch & !S4_dispatch, "S3",
                                               if_else(!S3_dispatch & S4_dispatch, "S4",
                                                       "Both")))) %>%
        group_by(dispatch_type) %>%
        summarize(argument_count = n()) %>%
        ungroup() %>%
        mutate(relative_argument_count = argument_count / sum(argument_count))

    escaped_argument_count_by_expression_type <-
        analyses$escaped_arguments %>%
        group_by(expression_type) %>%
        summarize(argument_count = n()) %>%
        ungroup() %>%
        mutate(relative_argument_count = argument_count / sum(argument_count))

    escaped_argument_count_by_value_type <-
        analyses$escaped_arguments %>%
        group_by(value_type) %>%
        summarize(argument_count = n()) %>%
        ungroup() %>%
        mutate(relative_argument_count = argument_count / sum(argument_count))


    summarize_before_and_after <- function(df, before_column, after_column, type_column) {
        before_column <- enquo(before_column)
        after_column <- enquo(after_column)
        type_column <- enquo(type_column)

        df <-
            df %>%
            mutate(!!type_column :=
                       if_else(!!before_column != 0 & !!after_column == 0, "Before",
                               if_else(!!before_column == 0 & !!after_column != 0, "After",
                                       if_else(!!before_column != 0 & !!after_column != 0, "Both",
                                               "Never")))) %>%
            group_by(!!type_column) %>%
            summarize(argument_count = n()) %>%
            ungroup() %>%
            mutate(relative_argument_count = argument_count / sum(argument_count))

        add_row_if_absent <- function(df, column_value) {
            if(!(column_value %in% colnames(df))) {
                df <-
                    df %>%
                    add_row(!!type_column := column_value,
                            argument_count = 0,
                            relative_argument_count = 0)
            }
            df
        }

        df %>%
            add_row_if_absent("Before") %>%
            add_row_if_absent("After") %>%
            add_row_if_absent("Both") %>%
            add_row_if_absent("Never")
    }

    escaped_argument_count_by_force_point <-
        analyses$escaped_arguments %>%
        summarize_before_and_after(before_escape_force_count,
                                   after_escape_force_count,
                                   point) %>%
        add_column(use = "Force", .before = 1)

    escaped_argument_count_by_lookup_point <-
        analyses$escaped_arguments %>%
        summarize_before_and_after(before_escape_value_lookup_count,
                                   after_escape_value_lookup_count,
                                   point) %>%
        add_column(use = "Lookup", .before = 1)

    escaped_argument_count_by_metaprogram_point <-
        analyses$escaped_arguments %>%
        summarize_before_and_after(before_escape_metaprogram_count,
                                   after_escape_metaprogram_count,
                                   point) %>%
        add_column(use = "Metaprogram", .before = 1)

    escaped_argument_count_by_use_point <-
        bind_rows(escaped_argument_count_by_force_point,
                  escaped_argument_count_by_lookup_point,
                  escaped_argument_count_by_metaprogram_point)

    escaped_argument_count_by_direct_self_scope_mutation_type <-
        analyses$escaped_arguments %>%
        summarize_before_and_after(before_escape_direct_self_scope_mutation_count,
                                   after_escape_direct_self_scope_mutation_count,
                                   direct_self_scope_mutation_type)

    escaped_argument_count_by_indirect_self_scope_mutation_type <-
        analyses$escaped_arguments %>%
        summarize_before_and_after(before_escape_indirect_self_scope_mutation_count,
                                   after_escape_indirect_self_scope_mutation_count,
                                   indirect_self_scope_mutation_type)

    escaped_argument_count_by_direct_lexical_scope_mutation_type <-
        analyses$escaped_arguments %>%
        summarize_before_and_after(before_escape_direct_lexical_scope_mutation_count,
                                   after_escape_direct_lexical_scope_mutation_count,
                                   direct_lexical_scope_mutation_type)

    escaped_argument_count_by_indirect_lexical_scope_mutation_type <-
        analyses$escaped_arguments %>%
        summarize_before_and_after(before_escape_indirect_lexical_scope_mutation_count,
                                   after_escape_indirect_lexical_scope_mutation_count,
                                   indirect_lexical_scope_mutation_type)

    escaped_argument_count_by_direct_non_lexical_scope_mutation_type <-
        analyses$escaped_arguments %>%
        summarize_before_and_after(before_escape_direct_non_lexical_scope_mutation_count,
                                   after_escape_direct_non_lexical_scope_mutation_count,
                                   direct_non_lexical_scope_mutation_type)

    escaped_argument_count_by_indirect_non_lexical_scope_mutation_type <-
        analyses$escaped_arguments %>%
        summarize_before_and_after(before_escape_indirect_non_lexical_scope_mutation_count,
                                   after_escape_indirect_non_lexical_scope_mutation_count,
                                   indirect_non_lexical_scope_mutation_type)

    escaped_argument_count_by_direct_self_scope_observation_type <-
        analyses$escaped_arguments %>%
        summarize_before_and_after(before_escape_direct_self_scope_observation_count,
                                   after_escape_direct_self_scope_observation_count,
                                   direct_self_scope_observation_type)

    escaped_argument_count_by_indirect_self_scope_observation_type <-
        analyses$escaped_arguments %>%
        summarize_before_and_after(before_escape_indirect_self_scope_observation_count,
                                   after_escape_indirect_self_scope_observation_count,
                                   indirect_self_scope_observation_type)

    escaped_argument_count_by_direct_lexical_scope_observation_type <-
        analyses$escaped_arguments %>%
        summarize_before_and_after(before_escape_direct_lexical_scope_observation_count,
                                   after_escape_direct_lexical_scope_observation_count,
                                   direct_lexical_scope_observation_type)

    escaped_argument_count_by_indirect_lexical_scope_observation_type <-
        analyses$escaped_arguments %>%
        summarize_before_and_after(before_escape_indirect_lexical_scope_observation_count,
                                   after_escape_indirect_lexical_scope_observation_count,
                                   indirect_lexical_scope_observation_type)

    escaped_argument_count_by_direct_non_lexical_scope_observation_type <-
        analyses$escaped_arguments %>%
        summarize_before_and_after(before_escape_direct_non_lexical_scope_observation_count,
                                   after_escape_direct_non_lexical_scope_observation_count,
                                   direct_non_lexical_scope_observation_type)

    escaped_argument_count_by_indirect_non_lexical_scope_observation_type <-
        analyses$escaped_arguments %>%
        summarize_before_and_after(before_escape_indirect_non_lexical_scope_observation_count,
                                   after_escape_indirect_non_lexical_scope_observation_count,
                                   indirect_non_lexical_scope_observation_type)

    list(escaped_arguments = escaped_arguments,
         escaped_argument_function_call_count_by_return_value_type = escaped_argument_function_call_count_by_return_value_type,
         not_escaped_argument_function_call_count_by_return_value_type = not_escaped_argument_function_call_count_by_return_value_type,
         function_call_count_by_escape_category_and_return_value_type = function_call_count_by_escape_category_and_return_value_type,
         escaped_argument_function_count_by_return_value_type = escaped_argument_function_count_by_return_value_type,
         escaped_argument_function_category = escaped_argument_function_category,
         escaped_argument_function_count_by_category = escaped_argument_function_count_by_category,
         escaped_argument_count_by_nature = escaped_argument_count_by_nature,
         escaped_argument_count_by_dispatch_type = escaped_argument_count_by_dispatch_type,
         escaped_argument_count_by_expression_type = escaped_argument_count_by_expression_type,
         escaped_argument_count_by_value_type = escaped_argument_count_by_value_type,
         escaped_argument_count_by_use_point = escaped_argument_count_by_use_point,
         escaped_argument_count_by_force_point = escaped_argument_count_by_force_point,
         escaped_argument_count_by_metaprogram_point = escaped_argument_count_by_metaprogram_point,
         escaped_argument_count_by_lookup_point = escaped_argument_count_by_lookup_point,
         escaped_argument_count_by_direct_self_scope_mutation_type = escaped_argument_count_by_direct_self_scope_mutation_type,
         escaped_argument_count_by_indirect_self_scope_mutation_type = escaped_argument_count_by_indirect_self_scope_mutation_type,
         escaped_argument_count_by_direct_lexical_scope_mutation_type = escaped_argument_count_by_direct_lexical_scope_mutation_type,
         escaped_argument_count_by_indirect_lexical_scope_mutation_type = escaped_argument_count_by_indirect_lexical_scope_mutation_type,
         escaped_argument_count_by_direct_non_lexical_scope_mutation_type = escaped_argument_count_by_direct_non_lexical_scope_mutation_type,
         escaped_argument_count_by_indirect_non_lexical_scope_mutation_type = escaped_argument_count_by_indirect_non_lexical_scope_mutation_type,
         escaped_argument_count_by_direct_self_scope_observation_type = escaped_argument_count_by_direct_self_scope_observation_type,
         escaped_argument_count_by_indirect_self_scope_observation_type = escaped_argument_count_by_indirect_self_scope_observation_type,
         escaped_argument_count_by_direct_lexical_scope_observation_type = escaped_argument_count_by_direct_lexical_scope_observation_type,
         escaped_argument_count_by_indirect_lexical_scope_observation_type = escaped_argument_count_by_indirect_lexical_scope_observation_type,
         escaped_argument_count_by_direct_non_lexical_scope_observation_type = escaped_argument_count_by_direct_non_lexical_scope_observation_type,
         escaped_argument_count_by_indirect_non_lexical_scope_observation_type = escaped_argument_count_by_indirect_non_lexical_scope_observation_type)

        ## escaped_argument_call_return_value_type = escaped_argument_call_return_value_type,
        ##  escaped_argument_function_category = escaped_argument_function_category,
        ##  escaped_argument_function_return_type_and_category = escaped_argument_function_return_type_and_category,
        ##  escaped_argument_function_count_distribution_by_return_type_and_category = escaped_argument_function_count_distribution_by_return_type_and_category)
}


function_definitions <- function(analyses) {

    encode_sequence <- function(seq) {
        str_c("(", str_c(seq, collapse = " "), ")", sep = "")
    }

    decode_sequence <- function(seq) {
        unlist(str_split(str_sub(seq, 2, -2), " "))
    }

    function_definitions_with_script  <-
        analyses$function_definitions %>%
        group_by(function_id) %>%
        summarize(function_name = encode_sequence(unique(decode_sequence(function_name))),
                  definition = first(definition),
                  script = encode_sequence(str_c(package, script_type, script_name,
                                                 sep = "/"))) %>%
        ungroup()

    function_definitions <-
        function_definitions_with_script %>%
        select(-script)

    list(function_definitions = function_definitions,
         function_definitions_with_script = function_definitions_with_script)
}


summarize_analyses  <- function(analyses) {

    compute_parameter_mode <- function(argument_mode, argument_use_count) {
        tibble(argument_mode, argument_use_count) %>%
            filter(argument_use_count != 0) %>%
            pull(argument_mode) %>%
            unique() %>%
            sort() %>%
            str_c(collapse = " or ")
    }

    compute_parameter_use_mode <- function(argument_use_mode, argument_use_count) {

        parameter_use_mode <-
            tibble(argument_use_mode, argument_use_count) %>%
            filter(argument_use_count != 0 &
                   argument_use_mode != "Force") %>%
            pull(argument_use_mode)

        if ("Lookup & Metaprogram" %in% parameter_use_mode) {
            parameter_use_mode <- c("Lookup",
                                    "Metaprogram",
                                    setdiff(parameter_use_mode,
                                            "Lookup & Metaprogram"))
        }
        str_c(sort(unique(parameter_use_mode)), collapse = " or ")
    }

    compute_parameter_class <- function(argument_use_mode, argument_use_count) {

        group <- tibble(argument_use_mode, argument_use_count)

        unused_count <-
            group %>%
            filter(argument_use_mode == "Unused") %>%
            pull(argument_use_count) %>%
            sum()

        used_count <-
            group %>%
            filter(argument_use_mode != "Unused" &
                   argument_use_mode != "Forced") %>%
            pull(argument_use_count) %>%
            sum()

        if (unused_count == 0 & used_count != 0) "Always"
        else if(unused_count != 0 & used_count == 0) "Never"
        else if(unused_count != 0 & used_count != 0) "Sometimes"
        else "Indeterminate"
    }


    compute_function_class <- function(parameter_class) {
        str_c(sort(unique(parameter_class)), collapse = " & ")
    }

    compute_laziness_class <- function(formal_parameter_count, force_order) {
        force_order_count <- length(force_order)
        force_order_argument_count =
            map(str_split(force_order, " \\| "),
                function(o) {
                    r <- purrr::discard(o, function(v) v == "")
                    length(r)
                })
        laziness_class = if(all(force_order_count == 1))
                             "Non Lazy"
                         else if(all(formal_parameter_count == force_order_argument_count))
                             "Nontrivially Lazy"
                         else if(any(formal_parameter_count == force_order_argument_count))
                             "Maybe Nontrivially Lazy"
                         else "Trivially Lazy"
        laziness_class
    }

    closures <-
        analyses$closures %>%
        mutate(function_name = str_split(function_name, " \\| ")) %>%
        group_by(function_id, missing_argument, force_order) %>%
        summarize(function_type = first(function_type),
                  force_order_call_count = sum(force_order_call_count),
                  formal_parameter_count = first(formal_parameter_count),
                  wrapper = all(wrapper),
                  function_name = list(unique(unlist(function_name)))) %>%
        ungroup() %>%
        group_by(function_id) %>%
        mutate(function_name = str_c(unique(unlist(function_name)), collapse = " "),
               force_order_count = length(unique(force_order)),
               non_missing_force_order_count = length(unique(force_order[!missing_argument])),
               call_count = sum(force_order_call_count),
               laziness_class = compute_laziness_class(formal_parameter_count, force_order)) %>%
        ungroup()

    once_called_functions <-
        closures %>%
        filter(call_count == 1 & formal_parameter_count > 0) %>%
        pull(function_id) %>%
        unique()

    parameters <-
        analyses$parameters %>%
        group_by(function_id, parameter_position, argument_mode, expression_type, value_type, argument_use_mode) %>%
        summarize(argument_use_count = sum(argument_use_count),
                  argument_count = sum(argument_count),
                  escape = any(escape),
                  source = str_c(unique(str_c(package, script_type, script_name, sep = "::")), collapse=" | ")) %>%
        ungroup() %>%
        group_by(function_id, parameter_position) %>%
        mutate(parameter_mode = compute_parameter_mode(argument_mode,
                                                       argument_use_count),
               parameter_use_mode = compute_parameter_use_mode(argument_use_mode,
                                                               argument_use_count),
               parameter_class = compute_parameter_class(argument_use_mode,
                                                         argument_use_count)) %>%
        ungroup() %>%
        group_by(function_id) %>%
        mutate(function_class = compute_function_class(parameter_class)) %>%
        ungroup()

    parameters_multiply_called_functions <-
        parameters %>%
        filter(!(function_id %in% once_called_functions))

    function_class_table <-
        parameters %>%
        group_by(function_id) %>%
        summarize(function_class = first(function_class))

    closures <-
        closures %>%
        left_join(function_class_table, by = "function_id") %>%
        mutate(function_class = ifelse(formal_parameter_count == 0,
                                       "Zero",
                                       function_class))

    argument_execution_time_by_parameter_class <-
        analyses$argument_execution_time %>%
        left_join(select(parameters, function_id, parameter_position, parameter_class),
                  by = c("function_id", "parameter_position")) %>%
        select(parameter_class, expression_type, execution_time, argument_count) %>%
        mutate(execution_time = as.integer(execution_time / 1000000)) %>%
        group_by(parameter_class, expression_type, execution_time) %>%
        summarize(argument_count = sum(argument_count)) %>%
        ungroup() %>%
        group_by(parameter_class) %>%
        mutate(relative_argument_count = argument_count / sum(argument_count)) %>%
        ungroup()

    escaped_arguments <-
        parameters %>%
        filter(escape) %>%
        group_by(function_id, parameter_position) %>%
        summarize(source = first(source),
                  count = sum(argument_count)) %>%
        ungroup()

    argument_count_by_argument_mode <-
        parameters_multiply_called_functions %>%
        filter(argument_use_mode != "Force") %>%
        group_by(argument_mode) %>%
        summarize(argument_count = sum(argument_use_count)) %>%
        mutate(total_argument_count = sum(argument_count)) %>%
        mutate(relative_argument_count = argument_count / total_argument_count)

    argument_count_by_argument_use_mode <-
        parameters_multiply_called_functions %>%
        filter(argument_use_mode != "Force") %>%
        group_by(argument_use_mode) %>%
        summarize(argument_count = sum(argument_use_count)) %>%
        mutate(total_argument_count = sum(argument_count)) %>%
        mutate(relative_argument_count = argument_count / total_argument_count)

    argument_count_by_argument_mode_and_argument_use_mode <-
        parameters_multiply_called_functions %>%
        filter(argument_use_mode != "Force") %>%
        group_by(argument_mode, argument_use_mode) %>%
        summarize(argument_count = sum(argument_use_count)) %>%
        ungroup() %>%
        mutate(total_argument_count = sum(argument_count)) %>%
        mutate(relative_argument_count = argument_count / total_argument_count)

    argument_count_by_argument_use_mode_and_argument_mode <-
        parameters_multiply_called_functions %>%
        filter(argument_use_mode != "Force") %>%
        group_by(argument_use_mode, argument_mode) %>%
        summarize(argument_count = sum(argument_use_count)) %>%
        ungroup() %>%
        mutate(total_argument_count = sum(argument_count)) %>%
        mutate(relative_argument_count = argument_count / total_argument_count)

    parameter_count_by_parameter_mode <-
        parameters_multiply_called_functions %>%
        group_by(function_id, parameter_position, parameter_mode) %>%
        summarize(parameter_count = 1) %>%
        ungroup() %>%
        group_by(parameter_mode) %>%
        summarize(parameter_count = sum(parameter_count)) %>%
        ungroup() %>%
        mutate(total_parameter_count = sum(parameter_count)) %>%
        mutate(relative_parameter_count = parameter_count / total_parameter_count)

    parameter_count_by_parameter_use_mode <-
        parameters_multiply_called_functions %>%
        group_by(function_id, parameter_position, parameter_use_mode) %>%
        summarize(parameter_count = 1) %>%
        group_by(parameter_use_mode) %>%
        summarize(parameter_count = sum(parameter_count)) %>%
        mutate(total_parameter_count = sum(parameter_count)) %>%
        mutate(relative_parameter_count = parameter_count / total_parameter_count)

    parameter_count_by_parameter_class <-
        parameters_multiply_called_functions %>%
        group_by(function_id, parameter_position, parameter_class) %>%
        summarize(parameter_count = 1) %>%
        group_by(parameter_class) %>%
        summarize(parameter_count = sum(parameter_count)) %>%
        mutate(total_parameter_count = sum(parameter_count)) %>%
        mutate(relative_parameter_count = parameter_count / total_parameter_count)

    parameter_count_by_parameter_mode_and_parameter_use_mode <-
        parameters_multiply_called_functions %>%
        group_by(function_id, parameter_position, parameter_mode, parameter_use_mode) %>%
        summarize(parameter_count = 1) %>%
        group_by(parameter_mode, parameter_use_mode) %>%
        summarize(parameter_count = sum(parameter_count)) %>%
        ungroup() %>%
        mutate(total_parameter_count = sum(parameter_count)) %>%
        mutate(relative_parameter_count = parameter_count / total_parameter_count)

    parameter_count_by_parameter_use_mode_and_parameter_mode <-
        parameters_multiply_called_functions %>%
        group_by(function_id, parameter_position, parameter_mode, parameter_use_mode) %>%
        summarize(parameter_count = 1) %>%
        group_by(parameter_use_mode, parameter_mode) %>%
        summarize(parameter_count = sum(parameter_count)) %>%
        ungroup() %>%
        mutate(total_parameter_count = sum(parameter_count)) %>%
        mutate(relative_parameter_count = parameter_count / total_parameter_count)

    parameter_count_by_parameter_mode_and_parameter_class <-
        parameters_multiply_called_functions %>%
        group_by(function_id, parameter_position, parameter_mode, parameter_class) %>%
        summarize(parameter_count = 1) %>%
        group_by(parameter_mode, parameter_class) %>%
        summarize(parameter_count = sum(parameter_count)) %>%
        ungroup() %>%
        mutate(total_parameter_count = sum(parameter_count)) %>%
        mutate(relative_parameter_count = parameter_count / total_parameter_count)

    parameter_count_by_parameter_class_and_parameter_mode <-
        parameters_multiply_called_functions %>%
        group_by(function_id, parameter_position, parameter_mode, parameter_class) %>%
        summarize(parameter_count = 1) %>%
        group_by(parameter_class, parameter_mode) %>%
        summarize(parameter_count = sum(parameter_count)) %>%
        ungroup() %>%
        mutate(total_parameter_count = sum(parameter_count)) %>%
        mutate(relative_parameter_count = parameter_count / total_parameter_count)

    parameter_count_by_parameter_use_mode_and_parameter_class <-
        parameters_multiply_called_functions %>%
        group_by(function_id, parameter_position, parameter_use_mode, parameter_class) %>%
        summarize(parameter_count = 1) %>%
        group_by(parameter_use_mode, parameter_class) %>%
        summarize(parameter_count = sum(parameter_count)) %>%
        ungroup() %>%
        mutate(total_parameter_count = sum(parameter_count)) %>%
        mutate(relative_parameter_count = parameter_count / total_parameter_count)

    parameter_count_by_parameter_class_and_parameter_use_mode <-
        parameters_multiply_called_functions %>%
        group_by(function_id, parameter_position, parameter_use_mode, parameter_class) %>%
        summarize(parameter_count = 1) %>%
        group_by(parameter_class, parameter_use_mode) %>%
        summarize(parameter_count = sum(parameter_count)) %>%
        ungroup() %>%
        mutate(total_parameter_count = sum(parameter_count)) %>%
        mutate(relative_parameter_count = parameter_count / total_parameter_count)

    argument_count_by_parameter_class_and_argument_mode_and_argument_use_mode <-
        parameters_multiply_called_functions %>%
        filter(argument_use_mode != "Force") %>%
        group_by(parameter_class, argument_mode, argument_use_mode) %>%
        summarize(argument_count = sum(argument_use_count)) %>%
        ungroup() %>%
        mutate(total_argument_count = sum(argument_count)) %>%
        mutate(relative_argument_count = argument_count / total_argument_count)


    total_function_count <- length(unique(closures$function_id))

    function_count_by_call_count <-
        closures %>%
        group_by(call_count) %>%
        summarize(function_count = length(unique(function_id))) %>%
        ungroup()

    frequently_called_function_count <-
        function_count_by_call_count %>%
        filter(call_count > 10) %>%
        pull(function_count) %>%
        sum()

    function_count_by_call_count <-
        function_count_by_call_count %>%
        mutate(call_count = as.character(call_count)) %>%
        add_row(call_count = "> 10", function_count = frequently_called_function_count) %>%
        mutate(total_function_count = total_function_count) %>%
        mutate(relative_function_count = function_count / total_function_count)

    function_count_by_formal_parameter_count <-
        closures %>%
        group_by(formal_parameter_count) %>%
        summarize(function_count = length(unique(function_id))) %>%
        ungroup()

    many_argument_function_count <-
        function_count_by_formal_parameter_count %>%
        filter(formal_parameter_count > 10) %>%
        pull(function_count) %>%
        sum()

    function_count_by_formal_parameter_count <-
        function_count_by_formal_parameter_count %>%
        mutate(formal_parameter_count = as.character(formal_parameter_count)) %>%
        add_row(formal_parameter_count = "> 10", function_count = many_argument_function_count) %>%
        mutate(total_function_count = total_function_count) %>%
        mutate(relative_function_count = function_count / total_function_count)

    function_count_by_wrapper_function_class_force_order_type_and_force_order_count <-
        closures %>%
        group_by(function_id) %>%
        summarize("Available or Missing" = first(force_order_count),
                  "Available" = first(non_missing_force_order_count),
                  formal_parameter_count = first(formal_parameter_count),
                  wrapper = first(wrapper),
                  function_class = first(function_class)) %>%
        ungroup() %>%
        gather(force_order_type, force_order_count, -function_id, -function_class, -formal_parameter_count, -wrapper) %>%
        group_by(wrapper, function_class, force_order_type, force_order_count) %>%
        summarize(function_count = n()) %>%
        ungroup()

    function_count_by_wrapper_force_order_type_and_force_order_count <-
        function_count_by_wrapper_function_class_force_order_type_and_force_order_count %>%
        group_by(wrapper, force_order_type, force_order_count) %>%
        summarize(function_count = sum(function_count)) %>%
        ungroup()

    many_force_order_count_function_count <-
        function_count_by_wrapper_force_order_type_and_force_order_count %>%
        filter(force_order_count > 5) %>%
        group_by(wrapper, force_order_type) %>%
        summarize(function_count = sum(function_count)) %>%
        ungroup() %>%
        mutate(force_order_count = "> 5")

    function_count_by_wrapper_force_order_type_and_force_order_count <-
        function_count_by_wrapper_force_order_type_and_force_order_count %>%
        mutate(force_order_count = as.character(force_order_count)) %>%
        rbind(many_force_order_count_function_count) %>%
        mutate(total_function_count = total_function_count) %>%
        mutate(relative_function_count = function_count / total_function_count)

    many_force_order_count_function_count <-
        function_count_by_wrapper_function_class_force_order_type_and_force_order_count %>%
        filter(force_order_count > 5) %>%
        group_by(wrapper, function_class, force_order_type, force_order_count) %>%
        summarize(function_count = sum(function_count)) %>%
        ungroup() %>%
        mutate(force_order_count = "> 5")

    function_count_by_wrapper_function_class_force_order_type_and_force_order_count <-
        function_count_by_wrapper_function_class_force_order_type_and_force_order_count %>%
        mutate(force_order_count = as.character(force_order_count)) %>%
        rbind(many_force_order_count_function_count) %>%
        mutate(total_function_count = total_function_count) %>%
        mutate(relative_function_count = function_count / total_function_count)

    total_argument_count <-
        analyses$argument_call_depth %>%
        pull(argument_count) %>%
        sum()

    argument_count_by_call_depth_and_expression_type <-
        analyses$argument_call_depth %>%
        group_by(call_depth, expression_type) %>%
        summarize(argument_count = sum(argument_count)) %>%
        ungroup()

    many_argument_call_depth <-
        argument_count_by_call_depth_and_expression_type %>%
        filter(call_depth > 10) %>%
        mutate(call_depth = "> 10") %>%
        group_by(call_depth, expression_type) %>%
        summarize(argument_count = sum(argument_count)) %>%
        ungroup()

    argument_count_by_call_depth_and_expression_type <-
        argument_count_by_call_depth_and_expression_type %>%
        mutate(call_depth = as.character(call_depth)) %>%
        rbind(many_argument_call_depth) %>%
        mutate(relative_argument_count = argument_count / total_argument_count)


    argument_count_by_promise_depth_and_expression_type <-
        analyses$argument_promise_depth %>%
        group_by(promise_depth, expression_type) %>%
        summarize(argument_count = sum(argument_count)) %>%
        ungroup()

    many_argument_promise_depth <-
        argument_count_by_promise_depth_and_expression_type %>%
        filter(promise_depth > 10) %>%
        mutate(promise_depth = "> 10") %>%
        group_by(promise_depth, expression_type) %>%
        summarize(argument_count = sum(argument_count)) %>%
        ungroup()

    argument_count_by_promise_depth_and_expression_type <-
        argument_count_by_promise_depth_and_expression_type %>%
        mutate(promise_depth = as.character(promise_depth)) %>%
        rbind(many_argument_promise_depth) %>%
        mutate(relative_argument_count = argument_count / total_argument_count)


    argument_count_by_nested_promise_depth_and_expression_type <-
        analyses$argument_nested_promise_depth %>%
        group_by(nested_promise_depth, expression_type) %>%
        summarize(argument_count = sum(argument_count)) %>%
        ungroup()

    many_argument_nested_promise_depth <-
        argument_count_by_nested_promise_depth_and_expression_type %>%
        filter(nested_promise_depth > 10) %>%
        mutate(nested_promise_depth = "> 10") %>%
        group_by(nested_promise_depth, expression_type) %>%
        summarize(argument_count = sum(argument_count)) %>%
        ungroup()

    argument_count_by_nested_promise_depth_and_expression_type <-
        argument_count_by_nested_promise_depth_and_expression_type %>%
        mutate(nested_promise_depth = as.character(nested_promise_depth)) %>%
        rbind(many_argument_nested_promise_depth) %>%
        mutate(relative_argument_count = argument_count / total_argument_count)

    list(closures = closures,
         once_called_functions = tibble(function_id = once_called_functions),
         parameters = parameters,
         argument_execution_time_by_parameter_class = argument_execution_time_by_parameter_class,
         escaped_arguments = escaped_arguments,
         parameters_multiply_called_functions = parameters_multiply_called_functions,
         argument_count_by_argument_mode = argument_count_by_argument_mode,
         argument_count_by_argument_use_mode = argument_count_by_argument_use_mode,
         argument_count_by_argument_mode_and_argument_use_mode = argument_count_by_argument_mode_and_argument_use_mode,
         argument_count_by_argument_use_mode_and_argument_mode = argument_count_by_argument_use_mode_and_argument_mode,
         parameter_count_by_parameter_mode = parameter_count_by_parameter_mode,
         parameter_count_by_parameter_use_mode = parameter_count_by_parameter_use_mode,
         parameter_count_by_parameter_class = parameter_count_by_parameter_class,
         parameter_count_by_parameter_mode_and_parameter_use_mode = parameter_count_by_parameter_mode_and_parameter_use_mode,
         parameter_count_by_parameter_use_mode_and_parameter_mode = parameter_count_by_parameter_use_mode_and_parameter_mode,
         parameter_count_by_parameter_mode_and_parameter_class = parameter_count_by_parameter_mode_and_parameter_class,
         parameter_count_by_parameter_class_and_parameter_mode = parameter_count_by_parameter_class_and_parameter_mode,
         parameter_count_by_parameter_use_mode_and_parameter_class = parameter_count_by_parameter_use_mode_and_parameter_class,
         parameter_count_by_parameter_class_and_parameter_use_mode = parameter_count_by_parameter_class_and_parameter_use_mode,
         argument_count_by_parameter_class_and_argument_mode_and_argument_use_mode = argument_count_by_parameter_class_and_argument_mode_and_argument_use_mode,
         function_count_by_call_count = function_count_by_call_count,
         function_count_by_formal_parameter_count = function_count_by_formal_parameter_count,
         function_count_by_wrapper_force_order_type_and_force_order_count = function_count_by_wrapper_force_order_type_and_force_order_count,
         function_count_by_wrapper_function_class_force_order_type_and_force_order_count = function_count_by_wrapper_function_class_force_order_type_and_force_order_count,
         argument_count_by_call_depth_and_expression_type = argument_count_by_call_depth_and_expression_type,
         argument_count_by_promise_depth_and_expression_type = argument_count_by_promise_depth_and_expression_type,
         argument_count_by_nested_promise_depth_and_expression_type = argument_count_by_nested_promise_depth_and_expression_type)
}


paper <- function(analyses) {

    split_pos_seq <- function(pos_seq) {
        map(str_split(str_sub(pos_seq, 2, -2), " "),
            function(chr_lst) if(length(chr_lst) == 1 && chr_lst == "") c() else chr_lst)
    }

    to_pos_seq <- function(pos_seqs) {
        map_chr(pos_seqs,
            function(pos_seq) str_c("(", str_c(pos_seq, collapse = " "), ")"))
    }

    pos_seq_eq <- function(pos_seq_1, pos_seq_2) {
        l1 <- length(pos_seq_1)
        l2 <- length(pos_seq_2)

        if(l1 != l2) {
            FALSE
        }
        else if(l1 == 0) {
            TRUE
        }
        else {
            all(pos_seq_1 == pos_seq_2)
        }
    }

    compute_strictness <- function(force_order, missing_arguments, formal_parameter_count) {
        force_order <- split_pos_seq(force_order)
        missing_arguments <- split_pos_seq(missing_arguments)
        all(lengths(force_order) + lengths(missing_arguments) >= formal_parameter_count)
    }

    ## TODO - investigate 'jhmb9cUOgugzW1R+979kzg==' - it has argument 1 in both missing and forced position

    ## closure_strictness <-
    ##     analyses$closure_force_order_count %>%
    ##     mutate(force_order = split_pos_seq(force_order),
    ##            missing_arguments = split_pos_seq(missing_arguments),
    ##            script = str_c(package, script_type, script_name, sep = "/")) %>%
    ##     group_by(script, wrapper, function_id) %>%
    ##     summarize(strict = all(lengths(force_order) + lengths(missing_arguments) >= formal_parameter_count),
    ##               formal_parameter_count = first(formal_parameter_count),
    ##               call_count = sum(call_count)) %>%
    ##     ungroup()

    ## closure_count_by_strictness_and_run <-
    ##     closure_strictness %>%
    ##     group_by(script, strict) %>%
    ##     summarize(function_count = length(unique(function_id))) %>%
    ##     mutate(relative_function_count = function_count / sum(function_count)) %>%
    ##     ungroup()


    extract_name_part <- function(function_names, pos) {
        function_names %>%
            map_chr(function(names) {
                names %>%
                    str_sub(2, -2) %>%
                    str_split(" ") %>%
                    unlist() %>%
                    str_split("::") %>%
                    map(function(pair) pair[pos]) %>%
                    unlist() %>%
                    unique()
                    #str_c(collapse = " ") %>%
                    #{str_c("(", ., ")")}
            })
    }

    join_names <- function(names) {
        splits <- unique(unlist(str_split(str_sub(names, 2, -2), " ")))
        str_c("(", str_c(splits, collapse = " "), ")")
    }

    extract_package_names <- function(function_name) {
        extract_name_part(function_name, 1)
    }

    extract_function_names <- function(function_name) {
        extract_name_part(function_name, 2)
    }

    closure_strictness <-
        analyses$closure_force_order_count %>%
        group_by(package_name, function_id) %>%
        do({
            ## get unique force_order and missing_argument combinations.
            formal_parameter_count <- .data$formal_parameter_count[1]
            call_count <- sum(as.double(.data$call_count))

            orders <-
                .data %>%
                group_by(force_order, missing_arguments) %>%
                summarize(count = n())

            force_order_seq <-
                orders %>%
                pull(force_order)

            force_order <-
                force_order_seq %>%
                str_c(collapse = " ") %>%
                {str_c("(", . , ")")}

            force_order_count <-
                length(unique(force_order_seq))

            missing_arguments <-
                orders %>%
                pull(missing_arguments) %>%
                str_c(collapse = " ") %>%
                {str_c("(", . , ")")}

            tibble(force_order = force_order,
                   missing_arguments = missing_arguments,
                   formal_parameter_count = formal_parameter_count,
                   call_count = call_count,
                   force_order_count = force_order_count,
                   strict = compute_strictness(orders$force_order,
                                               orders$missing_arguments,
                                               formal_parameter_count))
        }) %>%
        ungroup()

    package_strictness <-
        closure_strictness %>%
        group_by(package_name) %>%
        summarize(strict_function_count = sum(as.double(strict)),
                  non_strict_function_count = sum(as.double(!strict)),
                  total_function_count = n()) %>%
        ungroup() %>%
        mutate(relative_strict_function_count = strict_function_count / total_function_count,
               relative_lazy_function_count = non_strict_function_count / total_function_count) %>%
        arrange(desc(relative_strict_function_count))

    force_orders <-
        closure_strictness %>%
        filter(formal_parameter_count > 0 & call_count > 1) %>%
        group_by(force_order_count) %>%
        summarize(function_count = n()) %>%
        ungroup() %>%
        mutate(relative_function_count = function_count / sum(function_count))


    ### OJBECTS
    object_analyses <- objects(analyses)

    ### PROMISES
    promise_analyses <- promises(analyses)

    c(list(closure_strictness = closure_strictness,
           package_strictness = package_strictness,
           force_orders = force_orders),
      object_analyses,
      promise_analyses)

}


summarize_combined_data <- function(settings, combined_data_table) {

    info("=> Starting summarization\n")

    summarizer <- eval(as.symbol(settings$analysis))

    combined_data_filepaths <- combined_data_table$filepath

    summarized_data_filepaths <-
        combined_data_filepaths %>%
        path_ext_remove() %>%
        path_ext_remove()

    analyses <- new.env(parent = emptyenv(), hash = TRUE)

    map(summarized_data_filepaths,
        function(summarized_data_filepath) {
            delayedAssign(path_file(summarized_data_filepath),
                          read_data_table(summarized_data_filepath,
                                          binary = settings$binary,
                                          compression_level = settings$compression_level),
                          assign.env = analyses)
        })

    analyses %>%
        summarizer() %>%
        imap(
            function(df, name) {
                output_filepath <- path(settings$output_dirpath, name)

                info("=> Writing ", output_filepath, "\n")

                promisedyntracer::write_data_table(df, output_filepath,
                                                   truncate = TRUE,
                                                   binary = settings$binary,
                                                   compression_level = settings$compression_level)
                path(output_filepath,
                     ext = promisedyntracer::data_table_extension(settings$binary,
                                                                  settings$compression_level))
            }
        )

    info("=> Finished summarization\n\n")

    tibble(filepath = unlist(summarized_data_filepaths))
}


scan_input_dirpath <- function(settings) {

    info("=> Scanning for combined data files in ", settings$input_dirpath, "\n")

    combined_data_table <-
        settings$input_dirpath %>%
        dir_ls(type = "file") %>%
        {tibble(filepath = .)}

    info("=> Found ", nrow(combined_data_table), " data files\n")

    combined_data_table
}


parse_program_arguments <- function() {

    usage <- "%prog combined-output-dirpath summarized-output-dirpath"
    description <- paste(
        "combined-output-dirpath    directory containing combined data files",
        "summarized-output-dirpath  directory to which summarized data will be exported",
        "analysis                   name of analysis to run",
        sep = "\n")

    option_list <- list(
        make_option(c("--binary"),
                    action = "store_true",
                    default = FALSE,
                    help = "read data in binary format",
                    metavar = "binary"),

        make_option(c("--compression-level"),
                    action = "store",
                    type = "integer",
                    default = 0,
                    help = "compression level",
                    metavar = "compression_level")
    )

    option_parser <- OptionParser(usage = usage,
                                  description = description,
                                  add_help_option = TRUE,
                                  option_list = option_list)

    arguments <- parse_args2(option_parser)

    analysis_map <- list(
        object_type = list("object_count_distribution_by_type"),

        arguments = list("promise_distribution_by_source",
                         "argument_distribution_by_type",
                         "promise_argument_distribution_by_category",
                         "promise_distribution_by_sharing")
    )

    list(input_dirpath = arguments$args[1],
         output_dirpath = arguments$args[2],
         analysis = arguments$args[3],
         table_names = analysis_map[[arguments$args[3]]],
         binary = arguments$options$binary,
         compression_level = arguments$options$compression_level)

}


main <- function() {
    settings <- parse_program_arguments()

    dir_create(settings$output_dirpath)

    print(settings)

    combined_data_table <- scan_input_dirpath(settings)

    summarize_combined_data(settings, combined_data_table)

    invisible(NULL)
}


main()
