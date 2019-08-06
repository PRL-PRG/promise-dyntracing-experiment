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
suppressPackageStartupMessages(library(rlang))

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

verbatim <- function(analyses) {
    c(events(analyses),
      objects(analyses),
      escaped_arguments(analyses),
      function_definitions(analyses),
      side_effects(analyses),
      substitute_calls(analyses),
      promise_lifecycles(analyses),
      context_sensitive_lookups(analyses),
      promise_gc(analyses))
}

events <- function(analyses) {

    event_counts <-
        analyses$event_counts %>%
        mutate(script = enc2utf8(file.path(package, script_type, script_name))) %>%
        select(script, event, count)

    summarized_event_counts <-
        event_counts %>%
        group_by(event) %>%
        summarize(count = sum(as.double(count))) %>%
        ungroup()

    extreme_event_counts <-
        event_counts %>%
        group_by(event) %>%
        summarize(min_count = min(as.double(count)),
                  max_count = max(as.double(count))) %>%
        ungroup()

    event_processing_rates <-
        event_counts %>%
        group_by(script) %>%
        do({
            expression_count <- as.double(filter(.data, event == "EvalEntry")$count)
            event_count <- sum(as.double(filter(.data, event != "TracingTime")$count))
            tracing_time <- as.double(filter(.data, event == "TracingTime")$count)
             tibble("expression_count" = expression_count,
                    "event_count" = event_count,
                    "tracing_time" = tracing_time)
        }) %>%
        ungroup() %>%
        mutate(expression_rate = expression_count / tracing_time,
               event_rate = event_count / tracing_time)

    list(event_counts = event_counts,
         summarized_event_counts = summarized_event_counts,
         extreme_event_counts = extreme_event_counts,
         event_processing_rates = event_processing_rates)
}

substitute_calls <- function(analyses) {

    substitute_class_summary <-
        analyses$substitute_summaries %>%
        group_by(substitute_class) %>%
        summarize(call_count = sum(as.double(call_count))) %>%
        ungroup() %>%
        mutate(relative_call_count = call_count / sum(call_count))

    non_same_scope_substitute_calls <-
        analyses$substitute_summaries %>%
        filter(substitute_class != "SameScope") %>%
        group_by(substitute_class, caller_function_id, affected_function_id) %>%
        summarize(caller_function_namespace = first(caller_function_namespace),
                  caller_function_names = first(caller_function_names),
                  affected_function_namespace = first(affected_function_namespace),
                  affected_function_names = first(affected_function_names),
                  call_count = sum(as.double(call_count))) %>%
        ungroup() %>%
        arrange(desc(call_count))

    list(substitute_class_summary = substitute_class_summary,
         non_same_scope_substitute_calls = non_same_scope_substitute_calls)
}


promise_lifecycles <- function(analyses) {

    promise_lifecycles <-
        analyses$promise_lifecycles %>%
        filter(local)

    promise_lifecycle_summary <-
        promise_lifecycles %>%
        group_by(argument, escaped) %>%
        summarize(lifecycle_count = n_distinct(action, count),
                  promise_count = sum(as.double(promise_count))) %>%
        ungroup()

    promise_lifecycle_list <-
        promise_lifecycles %>%
        group_by(argument, escaped, action, count) %>%
        summarize(promise_count = sum(as.double(promise_count))) %>%
        ungroup() %>%
        group_by(argument, escaped) %>%
        mutate(relative_promise_count = promise_count / sum(promise_count)) %>%
        arrange(desc(promise_count)) %>%
        slice(1:20) %>%
        ungroup()

    list(promise_lifecycle_summary = promise_lifecycle_summary,
         promise_lifecycle_list = promise_lifecycle_list)
}

context_sensitive_lookups <- function(analyses) {
    context_sensitive_lookups <-
        analyses$context_sensitive_lookups %>%
        filter(local) %>%
        mutate(success = (value_type == "Closure" | value_type == "Special" | value_type == "Builtin"))

    context_sensitive_lookup_summary <-
        context_sensitive_lookups %>%
        group_by(success, argument) %>%
        summarize(forced = sum(as.double(forced)),
                  not_forced = sum(as.double(!forced)),
                  binding_lookup = sum(as.double(binding_lookup))) %>%
        ungroup()

    context_sensitive_lookup_symbols <-
        context_sensitive_lookups %>%
        filter(!success) %>%
        group_by(argument, value_type, function_id, formal_parameter_position, symbol) %>%
        summarize(package = first(package),
                  function_names = first(function_names),
                  promise_count = sum(as.double(n())),
                  binding_lookup = sum(as.double(binding_lookup))) %>%
        ungroup()

    list(context_sensitive_lookup_summary = context_sensitive_lookup_summary,
         context_sensitive_lookup_symbols = context_sensitive_lookup_symbols)
}

promise_gc <- function(analyses) {

    promise_gc <-
        analyses$promise_gc %>%
        filter(local) %>%
        mutate(single_gc_cycle = (gc_cycle_count == 1))

    promise_gc_summary <-
        promise_gc %>%
        group_by(argument, escaped, single_gc_cycle) %>%
        summarize(promise_count = sum(promise_count)) %>%
        ungroup() %>%
        mutate(relative_promise_count = promise_count / sum(promise_count)) %>%
        arrange(desc(promise_count))

    promise_gc_distribution_by_type <-
        promise_gc %>%
        group_by(single_gc_cycle, value_type) %>%
        summarize(promise_count = sum(promise_count)) %>%
        mutate(relative_promise_count = promise_count / sum(promise_count)) %>%
        ungroup()

    list(promise_gc_summary = promise_gc_summary,
         promise_gc_distribution_by_type = promise_gc_distribution_by_type)
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

side_effects <- function(analyses) {

    side_effects <-
        analyses$side_effects %>%
        group_by(function_id,
                 package,
                 function_name,
                 formal_parameter_position,
                 actual_argument_position,
                 creator,
                 mode,
                 direct,
                 expression) %>%
                 ##symbol) %>%
        summarize(count = sum(as.double(count))) %>%
        ungroup() %>%
        filter(!(package %in% c("base", "testthat"))) %>%
        filter(function_id != "<non-function-promise>")

    created_side_effects <-
        side_effects %>%
        filter(creator) %>%
        mutate(relative_count = count / sum(count)) %>%
        arrange(desc(count))


    observed_side_effects <-
        side_effects %>%
        filter(!creator) %>%
        mutate(relative_count = count / sum(count)) %>%
        arrange(desc(count))

    list(created_side_effects = created_side_effects,
         observed_side_effects = observed_side_effects)
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
        ungroup() %>%
        mutate(relative_closure_count = closure_count / sum(closure_count)) %>%
        summarize_outliers(call_count, closure_count, relative_closure_count, 10)

    closure_count_by_formal_parameter_count <-
        analyses$function_call_summary %>%
        filter(function_type == "Closure") %>%
        group_by(formal_parameter_count) %>%
        summarize(closure_count = length(unique(function_id))) %>%
        ungroup() %>%
        mutate(relative_closure_count = closure_count / sum(closure_count)) %>%
        summarize_outliers(formal_parameter_count, closure_count, relative_closure_count, 5)

    function_count_by_type <-
        analyses$function_call_summary %>%
        group_by(function_type) %>%
        summarize(function_count = length(unique(function_id))) %>%
        ungroup() %>%
        mutate(relative_function_count = function_count / sum(function_count))

    ## primitive_function_table <-
    ##     analyses$function_call_summary %>%
    ##     filter(function_type != "Closure") %>%
    ##     group_by(function_id, S3_method, S4_method, jumped, return_value_type) %>%
    ##     summarize(function_type = first(function_type),
    ##               call_count = sum(call_count)) %>%
    ##     ungroup()

    total_function_count <- sum(function_count_by_type$function_count)

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
        group_by(function_type, return_value_type) %>%
        summarize(function_count = length(unique(function_id))) %>%
        ungroup() %>%
        left_join(function_count_by_type %>%
                  select(function_type, total_function_count = function_count),
                  by = "function_type") %>%
        mutate(relative_function_count = function_count / total_function_count)

    join_names <- function(names) {
        splits <- unique(unlist(str_split(str_sub(names, 2, -2), " ")))
        str_c("(", str_c(splits, collapse = " "), ")")
    }

    function_call_summary <-
        analyses$function_call_summary %>%
        group_by(function_id, jumped, return_value_type) %>%
        summarize(package = first(package),
                  function_name = join_names(function_name),
                  function_type = first(function_type),
                  formal_parameter_count = first(formal_parameter_count),
                  S3_call_count = sum(as.double(S3_calls)),
                  S4_call_count = sum(as.double(S4_calls)),
                  call_count = sum(as.double(call_count))) %>%
        ungroup() %>%
        mutate(relative_call_count = call_count / sum(call_count))

    compute_method_type <- function(S3_call_count, S4_call_count) {
        S3_method <- as.logical(S3_call_count)
        S4_method <- as.logical(S4_call_count)

        if_else(!S3_method & !S4_method, "Ordinary",
        if_else(S3_method & !S4_method, "S3",
        if_else(!S3_method & S4_method, "S4", "S3 & S4")))
    }

    closure_method_type <-
        function_call_summary %>%
        filter(function_type == "Closure") %>%
        group_by(function_id) %>%
        summarize(S3_call_count = sum(S3_call_count),
                  S4_call_count = sum(S4_call_count),
                  call_count = sum(call_count)) %>%
        ungroup() %>%
        mutate(ordinary_call_count = call_count - S3_call_count - S4_call_count) %>%
        mutate(method_type = compute_method_type(S3_call_count, S4_call_count))

    closure_call_count_by_method_type <-
        tibble(call_type = c("S3", "S4", "Ordinary"),
               call_count = c(sum(closure_method_type$S3_call_count),
                              sum(closure_method_type$S4_call_count),
                              sum(closure_method_type$ordinary_call_count)))

    closure_count_by_method_type <-
        closure_method_type %>%
        group_by(method_type) %>%
        summarize(function_count = length(unique(function_id))) %>%
        ungroup()

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

    is_compatible <- function(order1, missing1, order2, missing2) {
        non_missing_order1 <- order1[! order1 %in% missing2]
        non_missing_order2 <- order2[! order2 %in% missing1]
        pos_seq_eq(non_missing_order1, non_missing_order2)
    }

    any_compatible <- function(force_order, missing_arguments,
                               compatible_force_order_list,
                               compatible_missing_arguments_list) {

        for(i in seq2(1, length(compatible_force_order_list))) {
            if(is_compatible(force_order,
                             missing_arguments,
                             compatible_force_order_list[[i]],
                             compatible_missing_arguments_list[[i]])) {
                return(TRUE)
            }
        }

        return(FALSE)
    }

    combine_force_order <- function(order_seq, deep = FALSE) {
        combiner <- function(elements) {
            str_c("(", str_c(elements, collapse = " "), ")")

        }
        if(deep) {
            order_seq <- map_chr(order_seq, combiner)
        }

        combiner(order_seq)
    }

    compute_strictness <- function(force_order, missing_arguments, formal_parameter_count) {
        all(lengths(force_order) + lengths(missing_arguments) >= formal_parameter_count)
    }

    compute_function_force_order <- function(data) {

        formal_parameter_count <- data$formal_parameter_count[1]
        call_count <- sum(as.double(data$call_count))
        wrapper <- any(data$wrapper)
        package <- first(data$package)

        orders <-
            data %>%
            group_by(force_order, missing_arguments) %>%
            summarize(count = n()) %>%
            ungroup()

        force_order_list <- split_pos_seq(orders$force_order)

        missing_arguments_list <- split_pos_seq(orders$missing_arguments)

        original_strict <- compute_strictness(force_order_list,
                                              missing_arguments_list,
                                              formal_parameter_count)

        compatible_force_order_list <- force_order_list[1]

        compatible_missing_arguments_list <- missing_arguments_list[1]

        for(i in seq2(2, length(force_order_list))) {

            force_order <- force_order_list[[i]]
            missing_arguments <- missing_arguments_list[[i]]
            compatible <- any_compatible(force_order,
                                         missing_arguments,
                                         compatible_force_order_list,
                                         compatible_missing_arguments_list)

            if(!compatible) {
                compatible_force_order_list <- c(list(force_order),
                                                 compatible_force_order_list)
                compatible_missing_arguments_list <- c(list(missing_arguments),
                                                       compatible_missing_arguments_list)
            }
        }

        strict <- compute_strictness(compatible_force_order_list,
                                     compatible_missing_arguments_list,
                                     formal_parameter_count)

        rbind(tibble(package_name = package,
                     wrapper = wrapper,
                     formal_parameter_count = formal_parameter_count,
                     call_count = call_count,
                     type = "original",
                     force_order = combine_force_order(orders$force_order, deep = FALSE),
                     missing_arguments = combine_force_order(orders$missing_arguments, deep = FALSE),
                     force_order_count = length(orders$force_order),
                     strict = original_strict),
              tibble(package_name = package,
                     wrapper = wrapper,
                     formal_parameter_count = formal_parameter_count,
                     call_count = call_count,
                     type = "compatible",
                     force_order = combine_force_order(compatible_force_order_list, deep = TRUE),
                     missing_arguments = combine_force_order(compatible_missing_arguments_list, deep = TRUE),
                     force_order_count = length(compatible_force_order_list),
                     strict = strict))
    }

    closure_strictness <-
        analyses$closure_force_order_count %>%
        group_by(function_id) %>%
        do(compute_function_force_order(.data)) %>%
        ungroup()

    closure_count_by_force_order_count <-
        closure_strictness %>%
        filter(call_count > 1 & formal_parameter_count > 0) %>%
        group_by(type, force_order_count) %>%
        summarize(function_count = n()) %>%
        mutate(relative_function_count = function_count / sum(function_count)) %>%
        group_by(type) %>%
        summarize_outliers(force_order_count, function_count, relative_function_count, 5) %>%
        ungroup()

    package_strictness <-
        closure_strictness %>%
        filter(call_count > 1 & formal_parameter_count > 0) %>%
        group_by(package_name, type) %>%
        summarize(uniorder_function_count = sum(force_order_count == 1),
                  multiorder_function_count = sum(force_order_count > 1),
                  strict_function_count = sum(as.double(strict)),
                  non_strict_function_count = sum(as.double(!strict)),
                  strict_uniorder_function_count = sum(strict & force_order_count == 1),
                  strict_multiorder_function_count = sum(strict & force_order_count > 1),
                  non_strict_uniorder_function_count = sum(!strict & force_order_count == 1),
                  non_strict_multiorder_function_count = sum(!strict & force_order_count > 1),
                  total_function_count = n()) %>%
        ungroup() %>%
        group_by(package_name, type) %>%
        mutate(relative_uniorder_function_count = uniorder_function_count / total_function_count,
               relative_multiorder_function_count = multiorder_function_count / total_function_count,
               relative_strict_function_count = strict_function_count / total_function_count,
               relative_non_strict_function_count = non_strict_function_count / total_function_count,
               relative_strict_uniorder_function_count = strict_uniorder_function_count / total_function_count,
               relative_strict_multiorder_function_count = strict_multiorder_function_count / total_function_count,
               relative_non_strict_uniorder_function_count = non_strict_uniorder_function_count / total_function_count,
               relative_non_strict_multiorder_function_count = non_strict_multiorder_function_count / total_function_count) %>%
        ungroup()


    list(closure_count_by_call_count = closure_count_by_call_count,
         function_count_by_type = function_count_by_type,
         #primitive_function_table = primitive_function_table,
         function_call_count_by_type = function_call_count_by_type,
         function_call_count_by_return_value_type = function_call_count_by_return_value_type,
         closure_call_count_by_return_value_type = closure_call_count_by_return_value_type,
         function_count_by_return_value_type = function_count_by_return_value_type,
         function_call_summary = function_call_summary,
         closure_method_type = closure_method_type,
         closure_call_count_by_method_type = closure_call_count_by_method_type,
         closure_count_by_method_type = closure_count_by_method_type,
         closure_strictness = closure_strictness,
         closure_count_by_force_order_count = closure_count_by_force_order_count,
         package_strictness = package_strictness)
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
        summarize(lookup = sum(as.double(lookup)),
                  metaprogram = sum(as.double(metaprogram)),
                  either = sum(as.double(either)),
                  argument_count = sum(as.double(argument_count)),
                  call_count = sum(as.double(call_count))) %>%
        ungroup() %>%
        mutate(lookup_class = classify_parameter(lookup, argument_count),
               metaprogram_class = classify_parameter(metaprogram, argument_count),
               either_class = classify_parameter(either, argument_count)) %>%
        mutate(parameter_use = if_else((lookup != 0) & (metaprogram == 0), "Lookup",
                               if_else((lookup == 0) & (metaprogram != 0), "Metaprogram",
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

    closure_metaprogramming <-
        formal_parameter_usage_class %>%
        group_by(function_id) %>%
        summarize(metaprogram = any(metaprogram != 0)) %>%
        ungroup()

    closure_count_by_metaprogramming <-
        tibble(metaprogram_category = c("Metaprogramming", "No Metaprogramming"),
               function_count = c(sum(as.double(closure_metaprogramming$metaprogram)),
                                  sum(as.double(!closure_metaprogramming$metaprogram)))) %>%
        mutate(relative_function_count = function_count / sum(function_count))

    package_metaprogramming <-
        closure_metaprogramming %>%
        left_join(select(analyses$function_definitions, function_id, package),
                  by = "function_id") %>%
        select(package, function_id, metaprogram) %>%
        group_by(package) %>%
        summarize(metaprogramming_function_count = sum(metaprogram),
                  non_metaprogramming_function_count = sum(!metaprogram),
                  total_function_count = n()) %>%
        ungroup() %>%
        mutate(relative_metaprogramming_function_count = metaprogramming_function_count / total_function_count,
               relative_non_metaprogramming_function_count = non_metaprogramming_function_count / total_function_count)

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


    argument_expression_types <-
        analyses$argument_expression_types %>%
        group_by(function_id, formal_parameter_position, expression_type) %>%
        summarize(count = sum(as.double(count))) %>%
        ungroup()


    argument_expression_type_by_usage_class <-
        argument_expression_types %>%
        left_join(select(formal_parameter_usage_class, function_id, formal_parameter_position, lookup_class),
                  by = c("function_id", "formal_parameter_position")) %>%
        group_by(lookup_class, expression_type) %>%
        summarize(count = sum(count)) %>%
        ungroup()

    list(argument_count_by_usage = argument_count_by_usage,
         formal_parameter_count_by_use = formal_parameter_count_by_use,
         formal_parameter_usage_class = formal_parameter_usage_class,
         formal_parameter_count_by_usage_class = formal_parameter_count_by_usage_class,
         closure_metaprogramming = closure_metaprogramming,
         closure_count_by_metaprogramming = closure_count_by_metaprogramming,
         package_metaprogramming = package_metaprogramming,
         closure_usage_class = closure_usage_class,
         closure_count_distribution_by_usage_class = closure_count_distribution_by_usage_class,
         argument_expression_types = argument_expression_types,
         argument_expression_type_by_usage_class = argument_expression_type_by_usage_class,
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

    argument_promise_count_by_forcing_scope <-
        analyses$argument_promise_count_by_forcing_scope %>%
        summarize_event_counts(forcing_scope)

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
                           5)

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
         argument_promise_count_by_forcing_scope = argument_promise_count_by_forcing_scope,
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

    if(is.null(analyses$escaped_arguments) || nrow(analyses$escaped_arguments) == 0) {
        return(list())
    }

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

    ## escaped_argument_count_by_dispatch_type <-
    ##     analyses$escaped_arguments %>%
    ##     mutate(dispatch_type = if_else(!S3_dispatch & !S4_dispatch, "Ordinary",
    ##                                    if_else(S3_dispatch & !S4_dispatch, "S3",
    ##                                            if_else(!S3_dispatch & S4_dispatch, "S4",
    ##                                                    "Both")))) %>%
    ##     group_by(dispatch_type) %>%
    ##     summarize(argument_count = n()) %>%
    ##     ungroup() %>%
    ##     mutate(relative_argument_count = argument_count / sum(argument_count))

    ## escaped_argument_count_by_expression_type <-
    ##     analyses$escaped_arguments %>%
    ##     group_by(expression_type) %>%
    ##     summarize(argument_count = n()) %>%
    ##     ungroup() %>%
    ##     mutate(relative_argument_count = argument_count / sum(argument_count))

    ## escaped_argument_count_by_value_type <-
    ##     analyses$escaped_arguments %>%
    ##     group_by(value_type) %>%
    ##     summarize(argument_count = n()) %>%
    ##     ungroup() %>%
    ##     mutate(relative_argument_count = argument_count / sum(argument_count))


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

    ## escaped_argument_count_by_direct_self_scope_mutation_type <-
    ##     analyses$escaped_arguments %>%
    ##     summarize_before_and_after(before_escape_direct_self_scope_mutation_count,
    ##                                after_escape_direct_self_scope_mutation_count,
    ##                                direct_self_scope_mutation_type)

    ## escaped_argument_count_by_indirect_self_scope_mutation_type <-
    ##     analyses$escaped_arguments %>%
    ##     summarize_before_and_after(before_escape_indirect_self_scope_mutation_count,
    ##                                after_escape_indirect_self_scope_mutation_count,
    ##                                indirect_self_scope_mutation_type)

    ## escaped_argument_count_by_direct_lexical_scope_mutation_type <-
    ##     analyses$escaped_arguments %>%
    ##     summarize_before_and_after(before_escape_direct_lexical_scope_mutation_count,
    ##                                after_escape_direct_lexical_scope_mutation_count,
    ##                                direct_lexical_scope_mutation_type)

    ## escaped_argument_count_by_indirect_lexical_scope_mutation_type <-
    ##     analyses$escaped_arguments %>%
    ##     summarize_before_and_after(before_escape_indirect_lexical_scope_mutation_count,
    ##                                after_escape_indirect_lexical_scope_mutation_count,
    ##                                indirect_lexical_scope_mutation_type)

    ## escaped_argument_count_by_direct_non_lexical_scope_mutation_type <-
    ##     analyses$escaped_arguments %>%
    ##     summarize_before_and_after(before_escape_direct_non_lexical_scope_mutation_count,
    ##                                after_escape_direct_non_lexical_scope_mutation_count,
    ##                                direct_non_lexical_scope_mutation_type)

    ## escaped_argument_count_by_indirect_non_lexical_scope_mutation_type <-
    ##     analyses$escaped_arguments %>%
    ##     summarize_before_and_after(before_escape_indirect_non_lexical_scope_mutation_count,
    ##                                after_escape_indirect_non_lexical_scope_mutation_count,
    ##                                indirect_non_lexical_scope_mutation_type)

    ## escaped_argument_count_by_direct_self_scope_observation_type <-
    ##     analyses$escaped_arguments %>%
    ##     summarize_before_and_after(before_escape_direct_self_scope_observation_count,
    ##                                after_escape_direct_self_scope_observation_count,
    ##                                direct_self_scope_observation_type)

    ## escaped_argument_count_by_indirect_self_scope_observation_type <-
    ##     analyses$escaped_arguments %>%
    ##     summarize_before_and_after(before_escape_indirect_self_scope_observation_count,
    ##                                after_escape_indirect_self_scope_observation_count,
    ##                                indirect_self_scope_observation_type)

    ## escaped_argument_count_by_direct_lexical_scope_observation_type <-
    ##     analyses$escaped_arguments %>%
    ##     summarize_before_and_after(before_escape_direct_lexical_scope_observation_count,
    ##                                after_escape_direct_lexical_scope_observation_count,
    ##                                direct_lexical_scope_observation_type)

    ## escaped_argument_count_by_indirect_lexical_scope_observation_type <-
    ##     analyses$escaped_arguments %>%
    ##     summarize_before_and_after(before_escape_indirect_lexical_scope_observation_count,
    ##                                after_escape_indirect_lexical_scope_observation_count,
    ##                                indirect_lexical_scope_observation_type)

    ## escaped_argument_count_by_direct_non_lexical_scope_observation_type <-
    ##     analyses$escaped_arguments %>%
    ##     summarize_before_and_after(before_escape_direct_non_lexical_scope_observation_count,
    ##                                after_escape_direct_non_lexical_scope_observation_count,
    ##                                direct_non_lexical_scope_observation_type)

    ## escaped_argument_count_by_indirect_non_lexical_scope_observation_type <-
    ##     analyses$escaped_arguments %>%
    ##     summarize_before_and_after(before_escape_indirect_non_lexical_scope_observation_count,
    ##                                after_escape_indirect_non_lexical_scope_observation_count,
    ##                                indirect_non_lexical_scope_observation_type)

    list(escaped_arguments = escaped_arguments,
         escaped_argument_function_call_count_by_return_value_type = escaped_argument_function_call_count_by_return_value_type,
         not_escaped_argument_function_call_count_by_return_value_type = not_escaped_argument_function_call_count_by_return_value_type,
         function_call_count_by_escape_category_and_return_value_type = function_call_count_by_escape_category_and_return_value_type,
         escaped_argument_function_count_by_return_value_type = escaped_argument_function_count_by_return_value_type,
         escaped_argument_function_category = escaped_argument_function_category,
         escaped_argument_function_count_by_category = escaped_argument_function_count_by_category,
         escaped_argument_count_by_nature = escaped_argument_count_by_nature,
         ## escaped_argument_count_by_dispatch_type = escaped_argument_count_by_dispatch_type,
         ## escaped_argument_count_by_expression_type = escaped_argument_count_by_expression_type,
         ## escaped_argument_count_by_value_type = escaped_argument_count_by_value_type,
         escaped_argument_count_by_use_point = escaped_argument_count_by_use_point,
         escaped_argument_count_by_force_point = escaped_argument_count_by_force_point,
         escaped_argument_count_by_metaprogram_point = escaped_argument_count_by_metaprogram_point,
         escaped_argument_count_by_lookup_point = escaped_argument_count_by_lookup_point)
         ## escaped_argument_count_by_direct_self_scope_mutation_type = escaped_argument_count_by_direct_self_scope_mutation_type,
         ## escaped_argument_count_by_indirect_self_scope_mutation_type = escaped_argument_count_by_indirect_self_scope_mutation_type,
         ## escaped_argument_count_by_direct_lexical_scope_mutation_type = escaped_argument_count_by_direct_lexical_scope_mutation_type,
         ## escaped_argument_count_by_indirect_lexical_scope_mutation_type = escaped_argument_count_by_indirect_lexical_scope_mutation_type,
         ## escaped_argument_count_by_direct_non_lexical_scope_mutation_type = escaped_argument_count_by_direct_non_lexical_scope_mutation_type,
         ## escaped_argument_count_by_indirect_non_lexical_scope_mutation_type = escaped_argument_count_by_indirect_non_lexical_scope_mutation_type,
         ## escaped_argument_count_by_direct_self_scope_observation_type = escaped_argument_count_by_direct_self_scope_observation_type,
         ## escaped_argument_count_by_indirect_self_scope_observation_type = escaped_argument_count_by_indirect_self_scope_observation_type,
         ## escaped_argument_count_by_direct_lexical_scope_observation_type = escaped_argument_count_by_direct_lexical_scope_observation_type,
         ## escaped_argument_count_by_indirect_lexical_scope_observation_type = escaped_argument_count_by_indirect_lexical_scope_observation_type,
         ## escaped_argument_count_by_direct_non_lexical_scope_observation_type = escaped_argument_count_by_direct_non_lexical_scope_observation_type,
         ## escaped_argument_count_by_indirect_non_lexical_scope_observation_type = escaped_argument_count_by_indirect_non_lexical_scope_observation_type
         ## )

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
        summarize(formal_parameter_count = first(formal_parameter_count),
                  package = first(package),
                  function_name = encode_sequence(unique(decode_sequence(function_name))),
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

argument_expression_types <- function(analyses) {
    argument_expression_types <-
        analyses$argument_expression_types %>%
        group_by(function_id, formal_parameter_position, expression_type) %>%
        summarize(count = sum(as.double(count))) %>%
        ungroup()

    argument_value_types <-
        analyses$argument_value_types %>%
        group_by(function_id, formal_parameter_position, value_type) %>%
        summarize(count = sum(as.double(count))) %>%
        ungroup()

    formal_parameter_usage_class <-
        analyses$formal_parameter_usage_class %>%
        select(function_id, formal_parameter_position, lookup_class)

    argument_expression_type_by_usage_class <-
        argument_expression_types %>%
        left_join(formal_parameter_usage_class,
                  by = c("function_id", "formal_parameter_position")) %>%
        group_by(lookup_class, expression_type) %>%
        summarize(count = sum(count)) %>%
        ungroup()

    argument_value_type_by_usage_class <-
        argument_value_types %>%
        left_join(formal_parameter_usage_class,
                  by = c("function_id", "formal_parameter_position")) %>%
        group_by(lookup_class, value_type) %>%
        summarize(count = sum(count)) %>%
        ungroup()

    list(argument_expression_types = argument_expression_types,
         argument_value_types = argument_value_types,
         argument_expression_type_by_usage_class = argument_expression_type_by_usage_class,
         argument_value_type_by_usage_class = argument_value_type_by_usage_class)
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


