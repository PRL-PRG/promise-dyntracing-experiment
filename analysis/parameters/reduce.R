#!/usr/bin/env Rscript

## WARN - no good way in R to find path of current script
##        perhaps the only good solution is to create a
##        package for the analysis ?

##force_order_count should be changed to fore_order_call_count
##also add force_order_count field to the main function table.

options(error = quote({dump.frames(to.file=FALSE); q();}))
options(tibble.width = Inf)

suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(optparse))
suppressPackageStartupMessages(library(magrittr))
suppressPackageStartupMessages(library(stringr))
suppressPackageStartupMessages(library(fs))
suppressPackageStartupMessages(library(promisedyntracer))
suppressPackageStartupMessages(library(purrr))
suppressPackageStartupMessages(library(lubridate))


source("analysis/utils.R")
source("analysis/analysis.R")


info <- function(...) cat((paste0(...)))


summarize_event_counts <- function(df,
                                   group_column,
                                   count_column) {
    group_column <- enquo(group_column)
    count_column <- enquo(count_column)

    group_column_values <-
        df %>%
        pull(!!group_column)

    tibble(!!group_column := group_column_values) %>%
        group_by(!!group_column) %>%
        summarize(!!count_column := n()) %>%
        ungroup()
}


verbatim <- function(analyses) {
    c(events(analyses),
      objects(analyses),
      escaped_arguments(analyses),
      function_definitions(analyses),
      promise_lifecycles(analyses),
      side_effects(analyses))
}


events <- function(analyses) {
    if(nrow(analyses$event_counts) == 0) {
        return(list())
    }

    begin <- as_datetime(analyses$BEGIN)
    finish <- as_datetime(analyses$FINISH)
    tracing_time <- as.double(int_length(interval(begin, finish)))

    event_counts <-
        analyses$event_counts %>%
        add_row(event = "TracingTime", count = tracing_time)

    ## event count is already summarized by the tracer.
    ## we only have to emit the same file again for summarization.
    list(event_counts = event_counts)
}


objects <- function(analyses) {

    if(nrow(analyses$object_counts) == 0) {
        return(list())
    }

    ## object type count is already summarized by the tracer.
    ## we only have to emit the same file again for summarization.
    list(object_counts = analyses$object_counts)
}


side_effects <- function(analyses) {

  if(nrow(analyses$side_effects) == 0) {
    return(list())
  }

  ## object type count is already summarized by the tracer.
  ## we only have to emit the same file again for summarization.
  list(side_effects = analyses$side_effects)
}


escaped_arguments <- function(analyses) {

    if(nrow(analyses$escaped_arguments) == 0) {
        return(list())
    }

    list(escaped_arguments = analyses$escaped_arguments)
}


function_definitions <- function(analyses) {

    if(nrow(analyses$function_definitions) == 0) {
        return(list())
    }

    list(function_definitions = analyses$function_definitions)
}


promise_lifecycles <- function(analyses) {

    if(nrow(analyses$promise_lifecycles) == 0) {
        return(list())
    }

    list(promise_lifecycles = analyses$promise_lifecycles)
}


functions <- function(analyses) {

    if(nrow(analyses$call_summaries) == 0) {
        return(list())
    }

    join_names <- function(names) {
        splits <- unique(unlist(str_split(str_sub(names, 2, -2), " ")))
        str_c("(", str_c(splits, collapse = " "), ")")
    }

    function_call_summary <-
        analyses$call_summaries %>%
        group_by(function_id, jumped, return_value_type) %>%
        summarize(package = first(package),
                  function_name = join_names(function_name),
                  function_type = first(function_type),
                  formal_parameter_count = first(formal_parameter_count),
                  S3_calls = sum(as.double(call_count[S3_method])),
                  S4_calls = sum(as.double(call_count[S4_method])),
                  call_count = sum(as.double(call_count))) %>%
        ungroup()

    ## only take into account closures that are not jumped
    closure_force_order_count <-
        analyses$call_summaries %>%
        filter(function_type == "Closure" & !jumped) %>%
        group_by(function_id, force_order, missing_arguments) %>%
        summarize(package = first(package),
                  wrapper = first(wrapper),
                  formal_parameter_count = first(formal_parameter_count),
                  call_count = sum(as.double(call_count)),
                  function_name = join_names(function_name)) %>%
        ungroup()

    list(function_call_summary = function_call_summary,
         closure_force_order_count = closure_force_order_count)
}


arguments <- function(analyses) {

    if(nrow(analyses$arguments) == 0) {
        return(list())
    }

    promise_arguments <-
        analyses$arguments %>%
        filter(argument_type == "Promise")

    argument_count_by_type <-
        analyses$arguments %>%
        summarize_event_counts(argument_type, argument_count)

    non_missing_dot_dot_dot <-
        analyses$arguments %>%
        filter(argument_type != "Missing") %>%
        select(call_id, formal_parameter_position, dot_dot_dot) %>%
        distinct(call_id, formal_parameter_position, dot_dot_dot) %>%
        pull(dot_dot_dot)

    non_missing_argument_count_by_dot_dot_dot <-
        tibble(dot_dot_dot = non_missing_dot_dot_dot) %>%
        summarize("Standard" = sum(!dot_dot_dot),
                  "Varargs" = sum(dot_dot_dot)) %>%
        gather(argument_category, argument_count)

    promise_argument_count_by_nature <-
        tibble(default = promise_arguments$default) %>%
        summarize("Non Default" = sum(!default),
                  "Default" = sum(default)) %>%
        gather(argument_nature, argument_count)

    promise_argument_count_by_sharing <-
        promise_arguments %>%
        summarize_event_counts(value_id, sharing_count) %>%
        group_by(sharing_count) %>%
        summarize(promise_count = n()) %>%
        ungroup()

    ## promise_argument_count_by_expression_type <-
    ##     promise_arguments %>%
    ##     summarize_event_counts(expression_type, argument_count)

    ## promise_argument_count_by_value_type <-
    ##     promise_arguments %>%
    ##     summarize_event_counts(value_type, argument_count)

    direct_force <- promise_arguments$direct_force
    indirect_force <- promise_arguments$indirect_force

    promise_argument_count_by_force_type <-
        tibble(force_type = c("Direct", "Indirect", "Direct & Indirect", "None"),
               argument_count = c(sum(direct_force & !indirect_force),
                                  sum(!direct_force & indirect_force),
                                  sum(direct_force & indirect_force),
                                  sum(!direct_force & !indirect_force)))

    ## promise_argument_count_by_direct_lookup_count <-
    ##     tibble(direct_lookup_count =
    ##                promise_arguments$direct_lookup_count) %>%
    ##     group_by(direct_lookup_count) %>%
    ##     summarize(argument_count = n())

    ## promise_argument_count_by_direct_and_indirect_lookup_count <-
    ##     tibble(direct_and_indirect_lookup_count =
    ##                promise_arguments$direct_lookup_count +
    ##                promise_arguments$indirect_lookup_count) %>%
    ##     group_by(direct_and_indirect_lookup_count) %>%
    ##     summarize(argument_count = n())

    ## promise_argument_count_by_direct_metaprogram_count <-
    ##     tibble(direct_metaprogram_count =
    ##                promise_arguments$direct_metaprogram_count) %>%
    ##     group_by(direct_metaprogram_count) %>%
    ##     summarize(argument_count = n())

    ## promise_argument_count_by_direct_and_indirect_metaprogram_count <-
    ##     tibble(direct_and_indirect_metaprogram_count =
    ##                promise_arguments$direct_metaprogram_count +
    ##                promise_arguments$indirect_metaprogram_count) %>%
    ##     group_by(direct_and_indirect_metaprogram_count) %>%
    ##     summarize(argument_count = n())

    ## S3_dispatch <- promise_arguments$S3_dispatch
    ## S4_dispatch <- promise_arguments$S4_dispatch

    ## promise_argument_count_by_dispatch_type <-
    ##     tibble(dispatch_type = c("S3", "S4", "S3 & S4", "None"),
    ##            argument_count = c(sum(S3_dispatch & !S4_dispatch),
    ##                               sum(!S3_dispatch & S4_dispatch),
    ##                               sum(S3_dispatch & S4_dispatch),
    ##                               sum(!S3_dispatch & !S4_dispatch)))

    promise_argument_forced_by_promise_argument <-
        promise_arguments %>%
        filter(forcing_actual_argument_position > -1) %>%
        select(function_id,
               formal_parameter_position,
               actual_argument_position,
               default,
               forcing_actual_argument_position)

    promise_argument_returning_non_locally <-
        promise_arguments %>%
        filter(non_local_return) %>%
        select(function_id,
               formal_parameter_position,
               actual_argument_position,
               default,
               expression_type,
               value_type)

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

    if(nrow(analyses$arguments) == 0) {
        return(list())
    }

    ## for each parameter position, we compute 4 points:
    ## - lookup
    ## - metaprogram
    ## - lookup & metaprogram
    ## - lookup | metaprogram
    formal_parameter_usage_counts <-
        analyses$arguments %>%
        mutate(direct_force = if_else(argument_type != "Promise", TRUE, direct_force)) %>%
        mutate(lookup = (as.logical(direct_lookup_count + indirect_lookup_count)
                         | direct_force | indirect_force),
               metaprogram = as.logical(direct_metaprogram_count + indirect_metaprogram_count)) %>%
        select(call_id, function_id, formal_parameter_position,
               lookup, metaprogram) %>%
        group_by(function_id, formal_parameter_position) %>%
        summarize(either = sum(metaprogram | lookup),
                  both = sum(metaprogram & lookup),
                  lookup = sum(lookup),
                  metaprogram = sum(metaprogram),
                  argument_count = n()) %>%
        mutate(call_count = n()) %>%
        ungroup()

    execution_times <-
        analyses$arguments %>%
        filter(argument_type == "Promise" & direct_force != 0 & execution_time >= 1000000) %>%
        select(function_id, formal_parameter_position, execution_time) %>%
        mutate(execution_time = round(execution_time / 1000000, digits = 1)) %>%
        group_by(function_id, formal_parameter_position, execution_time) %>%
        summarize(argument_count = 1.0 * n()) %>%
        ungroup()

    list(formal_parameter_usage_counts = formal_parameter_usage_counts,
         execution_times = execution_times)
}

argument_expression_types <- function(analyses) {
    if(nrow(analyses$arguments) == 0) {
        return(list())
    }

    promise_arguments <-
        analyses$arguments %>%
        filter(argument_type == "Promise")

    argument_expression_types <-
        promise_arguments %>%
        group_by(function_id, formal_parameter_position, expression_type) %>%
        summarize(count = n()) %>%
        ungroup()

    argument_value_types <-
        promise_arguments %>%
        group_by(function_id, formal_parameter_position, value_type) %>%
        summarize(count = n()) %>%
        ungroup()

    list(argument_expression_types = argument_expression_types,
         argument_value_types = argument_value_types)
}

promises <- function(analyses) {

    if(nrow(analyses$promises) == 0) {
        return(list())
    }

    argument_promises <-
        analyses$promises %>%
        filter(argument)

    non_argument_promises <-
        analyses$promises %>%
        filter(!argument)

    promise_count_by_category <-
        tibble(
            promise_category = c("Non Argument", "Argument"),
            promise_count = c(nrow(non_argument_promises), nrow(argument_promises))
        )

    argument_promise_count_by_expression_type <-
        argument_promises %>%
        summarize_event_counts(expression_type,
                               promise_count)

    ## non_argument_promise_count_by_expression_type <-
    ##     non_argument_promises %>%
    ##     summarize_event_counts(expression_type,
    ##                            promise_count)

    argument_promise_count_by_value_type <-
        argument_promises %>%
        summarize_event_counts(value_type,
                               promise_count)

    ## non_argument_promise_count_by_value_type <-
    ##     non_argument_promises %>%
    ##     summarize_event_counts(value_type,
    ##                            promise_count)

    ## argument_promise_count_by_creation_scope <-
    ##     argument_promises %>%
    ##     summarize_event_counts(creation_scope,
    ##                            promise_count)

    ## non_argument_promise_count_by_creation_scope <-
    ##     non_argument_promises %>%
    ##     summarize_event_counts(creation_scope,
    ##                            promise_count)

    ## argument_promise_count_by_forcing_scope <-
    ##     argument_promises %>%
    ##     summarize_event_counts(forcing_scope,
    ##                            promise_count)

    ## non_argument_promise_count_by_forcing_scope <-
    ##     non_argument_promises %>%
    ##     summarize_event_counts(forcing_scope,
    ##                            promise_count)

    S3_dispatch <- argument_promises$S3_dispatch
    S4_dispatch <- argument_promises$S4_dispatch

    argument_promise_count_by_dispatch_type <-
        tibble(dispatch_type = c("S3", "S4", "S3 & S4", "None"),
               promise_count = c(sum(S3_dispatch & !S4_dispatch),
                                 sum(!S3_dispatch & S4_dispatch),
                                 sum(S3_dispatch & S4_dispatch),
                                 sum(!S3_dispatch & !S4_dispatch)))

    argument_promise_count_by_call_depth <-
        argument_promises %>%
        summarize_event_counts(call_depth,
                               promise_count)

    ## argument_promise_count_by_promise_depth <-
    ##     argument_promises %>%
    ##     summarize_event_counts(promise_depth,
    ##                            promise_count)

    ## argument_promise_count_by_nested_promise_depth <-
    ##     argument_promises %>%
    ##     summarize_event_counts(nested_promise_depth,
    ##                            promise_count)

    argument_promise_count_by_force_count <-
        argument_promises %>%
        summarize_event_counts(force_count,
                               promise_count)

    argument_promise_count_by_metaprogram_count <-
        argument_promises %>%
        summarize_event_counts(metaprogram_count,
                               promise_count)

    argument_promise_count_by_value_lookup_count <-
        argument_promises %>%
        mutate(value_lookup_count = value_lookup_count + force_count) %>%
        summarize_event_counts(value_lookup_count,
                               promise_count)

    ## argument_promise_count_by_value_assign_count <-
    ##     argument_promises %>%
    ##     summarize_event_counts(value_assign_count,
    ##                            promise_count)

    argument_promise_count_by_expression_lookup_count <-
        argument_promises %>%
        summarize_event_counts(expression_lookup_count,
                               promise_count)

    ## argument_promise_count_by_expression_assign_count <-
    ##     argument_promises %>%
    ##     summarize_event_counts(expression_assign_count,
    ##                            promise_count)

    ## argument_promise_count_by_environment_lookup_count <-
    ##     argument_promises %>%
    ##     summarize_event_counts(environment_lookup_count,
    ##                            promise_count)

    ## argument_promise_count_by_environment_assign_count <-
    ##     argument_promises %>%
    ##     summarize_event_counts(environment_assign_count,
    ##                            promise_count)


    ## summarize_side_effect <- function(df, side_effect_column, type_column) {
    ##     side_effect_column <- enquo(side_effect_column)

    ##     total_argument_promise_count <-
    ##         nrow(df)

    ##     side_effect_promise_count <-
    ##         df %>%
    ##         pull(!!side_effect_column) %>%
    ##         as.logical() %>%
    ##         sum()

    ##     no_side_effect_promise_count <-
    ##         total_argument_promise_count - side_effect_promise_count

    ##     tibble(c("Yes", "No"),
    ##            c(side_effect_promise_count, no_side_effect_promise_count)) %>%
    ##         `colnames<-`(c(type_column, "promise_count"))
    ## }

    ## promise_count_by_direct_self_scope_mutation <-
    ##     argument_promises %>%
    ##     summarize_side_effect(direct_self_scope_mutation_count,
    ##                           "direct_self_scope_mutation")

    ## promise_count_by_indirect_self_scope_mutation <-
    ##     argument_promises %>%
    ##     summarize_side_effect(indirect_self_scope_mutation_count,
    ##                           "indirect_self_scope_mutation")

    ## promise_count_by_direct_lexical_scope_mutation <-
    ##     argument_promises %>%
    ##     summarize_side_effect(direct_lexical_scope_mutation_count,
    ##                           "direct_lexical_scope_mutation")

    ## promise_count_by_indirect_lexical_scope_mutation <-
    ##     argument_promises %>%
    ##     summarize_side_effect(indirect_lexical_scope_mutation_count,
    ##                           "indirect_lexical_scope_mutation")

    ## promise_count_by_direct_non_lexical_scope_mutation <-
    ##     argument_promises %>%
    ##     summarize_side_effect(direct_non_lexical_scope_mutation_count,
    ##                           "direct_non_lexical_scope_mutation")

    ## promise_count_by_indirect_non_lexical_scope_mutation <-
    ##     argument_promises %>%
    ##     summarize_side_effect(indirect_non_lexical_scope_mutation_count,
    ##                           "indirect_non_lexical_scope_mutation")

    ## promise_count_by_direct_self_scope_observation <-
    ##     argument_promises %>%
    ##     summarize_side_effect(direct_self_scope_observation_count,
    ##                           "direct_self_scope_observation")

    ## promise_count_by_indirect_self_scope_observation <-
    ##     argument_promises %>%
    ##     summarize_side_effect(indirect_self_scope_observation_count,
    ##                           "indirect_self_scope_observation")

    ## promise_count_by_direct_lexical_scope_observation <-
    ##     argument_promises %>%
    ##     summarize_side_effect(direct_lexical_scope_observation_count,
    ##                           "direct_lexical_scope_observation")

    ## promise_count_by_indirect_lexical_scope_observation <-
    ##     argument_promises %>%
    ##     summarize_side_effect(indirect_lexical_scope_observation_count,
    ##                           "indirect_lexical_scope_observation")

    ## promise_count_by_direct_non_lexical_scope_observation <-
    ##     argument_promises %>%
    ##     summarize_side_effect(direct_non_lexical_scope_observation_count,
    ##                           "direct_non_lexical_scope_observation")

    ## promise_count_by_indirect_non_lexical_scope_observation <-
    ##     argument_promises %>%
    ##     summarize_side_effect(indirect_non_lexical_scope_observation_count,
    ##                           "indirect_non_lexical_scope_observation")

    summarize_use <- function(table) {
        lookup_promise_count <-
            table %>%
            filter(lookup & !metaprogram) %>%
            nrow()

        metaprogrammed_promise_count <-
            table %>%
            filter(!lookup & metaprogram) %>%
            nrow()

        lookup_and_metaprogrammed_promise_count <-
            table %>%
            filter(lookup & metaprogram) %>%
            nrow()

        unused_promise_count <-
            table %>%
            filter(!lookup & !metaprogram) %>%
            nrow()

        tibble(use = c("Lookup", "Metaprogram", "Lookup & Metaprogram", "Unused"),
               promise_count = c(lookup_promise_count,
                                 metaprogrammed_promise_count,
                                 lookup_and_metaprogrammed_promise_count,
                                 unused_promise_count))
    }

    summarize_action <- function(table) {

        preforced_promise_count <-
            table %>%
            filter(preforce & !force) %>%
            nrow()

        forced_promise_count <-
            table %>%
            filter(!preforce & force) %>%
            nrow()

        preforced_and_forced_promise_count <-
            table %>%
            filter(preforce & force) %>%
            nrow()

        nonforced_promise_count <-
            table %>%
            filter(!preforce & !force) %>%
            nrow()

        tibble(action = c("Preforce", "Force", "Preforce & Force", "Not Forced"),
               promise_count = c(preforced_promise_count,
                                 forced_promise_count,
                                 preforced_and_forced_promise_count,
                                 nonforced_promise_count))
    }

    argument_promises <-
        argument_promises %>%
        mutate(force = as.logical(force_count),
               lookup = as.logical(value_lookup_count),
               metaprogram = as.logical(metaprogram_count)) %>%
        mutate(lookup = force | lookup) %>%
        select(force, lookup, metaprogram, preforce)

    argument_promise_use_distribution <-
        argument_promises %>%
        summarize_use() %>%
        ungroup()

    argument_promise_action_distribution <-
        argument_promises %>%
        summarize_action() %>%
        ungroup()

    list(promise_count_by_category = promise_count_by_category,
         argument_promise_count_by_expression_type = argument_promise_count_by_expression_type,
         #non_argument_promise_count_by_expression_type = non_argument_promise_count_by_expression_type,
         argument_promise_count_by_value_type = argument_promise_count_by_value_type,
         #non_argument_promise_count_by_value_type = non_argument_promise_count_by_value_type,
         #argument_promise_count_by_creation_scope = argument_promise_count_by_creation_scope,
         #non_argument_promise_count_by_creation_scope = non_argument_promise_count_by_creation_scope,
         #argument_promise_count_by_forcing_scope = argument_promise_count_by_forcing_scope,
         #non_argument_promise_count_by_forcing_scope = non_argument_promise_count_by_forcing_scope,
         argument_promise_count_by_dispatch_type = argument_promise_count_by_dispatch_type,
         argument_promise_count_by_call_depth = argument_promise_count_by_call_depth,
         # argument_promise_count_by_promise_depth = argument_promise_count_by_promise_depth,
         # argument_promise_count_by_nested_promise_depth = argument_promise_count_by_nested_promise_depth,
         argument_promise_count_by_force_count = argument_promise_count_by_force_count,
         argument_promise_count_by_metaprogram_count = argument_promise_count_by_metaprogram_count,
         argument_promise_count_by_value_lookup_count = argument_promise_count_by_value_lookup_count,
         # argument_promise_count_by_value_assign_count = argument_promise_count_by_value_assign_count,
         argument_promise_count_by_expression_lookup_count = argument_promise_count_by_expression_lookup_count,
         # argument_promise_count_by_expression_assign_count = argument_promise_count_by_expression_assign_count,
         # argument_promise_count_by_environment_lookup_count = argument_promise_count_by_environment_lookup_count,
         # argument_promise_count_by_environment_assign_count = argument_promise_count_by_environment_assign_count,
         ##promise_count_by_direct_self_scope_mutation = promise_count_by_direct_self_scope_mutation,
         ##promise_count_by_indirect_self_scope_mutation = promise_count_by_indirect_self_scope_mutation,
         ##promise_count_by_direct_lexical_scope_mutation = promise_count_by_direct_lexical_scope_mutation,
         ##promise_count_by_indirect_lexical_scope_mutation = promise_count_by_indirect_lexical_scope_mutation,
         ##promise_count_by_direct_non_lexical_scope_mutation = promise_count_by_direct_non_lexical_scope_mutation,
         ##promise_count_by_indirect_non_lexical_scope_mutation = promise_count_by_indirect_non_lexical_scope_mutation,
         ##promise_count_by_direct_self_scope_observation = promise_count_by_direct_self_scope_observation,
         ##promise_count_by_indirect_self_scope_observation = promise_count_by_indirect_self_scope_observation,
         ##promise_count_by_direct_lexical_scope_observation = promise_count_by_direct_lexical_scope_observation,
         ##promise_count_by_indirect_lexical_scope_observation = promise_count_by_indirect_lexical_scope_observation,
         ##promise_count_by_direct_non_lexical_scope_observation = promise_count_by_direct_non_lexical_scope_observation,
         ##promise_count_by_indirect_non_lexical_scope_observation = promise_count_by_indirect_non_lexical_scope_observation,
         argument_promise_use_distribution = argument_promise_use_distribution,
         argument_promise_action_distribution = argument_promise_action_distribution)
}


reduce_analysis <- function(analyses) {

    ## This is 1ms execution time represented in ns.
    ## Since the tracer outputs promise execution time
    ## in ns, this number is represented in ns.
    ## This magic figure is a heuristic. We believe
    ## arguments that are expensive to evaluate should
    ## take at least 1ms to execute. This is used later
    ## to filter all promises that take more than a ms to execute.
    MINIMUM_EXPENSIVE_ARGUMENT_EXECUTION_TIME <- 1000000

    parameters <-
        analyses$arguments %>%
        # filter(parameter_mode != "Unknown") %>%
        group_by(function_id, parameter_position, argument_mode, expression_type, value_type) %>%
        summarize(argument_count = n(),
                  escape = any(as.logical(escape)),
                  "Force" = sum(as.logical(force_count)),
                  "Lookup" = sum(as.logical(lookup_count) & !as.logical(metaprogram_count)),
                  "Metaprogram" = sum(as.logical(metaprogram_count) & !as.logical(lookup_count)),
                  "Unused" = sum(!as.logical(metaprogram_count) & !as.logical(lookup_count)),
                  "Lookup & Metaprogram" = sum(as.logical(metaprogram_count) & as.logical(lookup_count))) %>%
        ungroup() %>%
        gather(argument_use_mode, argument_use_count,
               -function_id, -parameter_position,
               -argument_mode, -expression_type,
               -value_type, -argument_count, -escape)

    calls_with_missing_arguments <-
        analyses$arguments %>%
        group_by(call_id) %>%
        summarize(missing_argument = "Missing" %in% argument_mode) %>%
        ungroup()

    internal_call_id <-
        analyses$calls %>%
        filter(function_name == ".Internal") %>%
        pull(call_id)

    wrapper_id <-
        analyses$`call-graph` %>%
        group_by(caller_id) %>%
        summarize(calls = n(), callee_id = first(callee_id)) %>%
        ungroup() %>%
        filter(calls == 1 & callee_id %in% internal_call_id) %>%
        pull(caller_id)

    closures <-
        analyses$calls %>%
        filter(function_type == "Closure") %>%
        left_join(calls_with_missing_arguments, by = "call_id") %>%
        group_by(function_id, missing_argument, force_order) %>%
        summarize(force_order_call_count = n(),
                  function_type = first(function_type),
                  formal_parameter_count = first(formal_parameter_count),
                  function_name = str_c(unique(function_name), collapse = " | "),
                  wrapper = all(call_id %in% wrapper_id)) %>%
        ungroup()

    argument_execution_time <-
        analyses$arguments %>%
        filter(force_count > 0) %>%
        filter(execution_time > MINIMUM_EXPENSIVE_ARGUMENT_EXECUTION_TIME) %>%
        group_by(function_id, parameter_position, expression_type, execution_time) %>%
        summarize(argument_count = 1.0 * n()) %>%
        ungroup()

    argument_call_depth <-
        analyses$arguments %>%
        select(call_depth, expression_type) %>%
        group_by(call_depth, expression_type) %>%
        summarize(argument_count = n()) %>%
        ungroup()

    argument_promise_depth <-
        analyses$arguments %>%
        select(promise_depth, expression_type) %>%
        group_by(promise_depth, expression_type) %>%
        summarize(argument_count = n()) %>%
        ungroup()

    argument_nested_promise_depth <-
        analyses$arguments %>%
        select(nested_promise_depth, expression_type) %>%
        group_by(nested_promise_depth, expression_type) %>%
        summarize(argument_count = n()) %>%
        ungroup()

    list(parameters = parameters,
         closures = closures,
         argument_execution_time = argument_execution_time,
         argument_call_depth = argument_call_depth,
         argument_promise_depth = argument_promise_depth,
         argument_nested_promise_depth = argument_nested_promise_depth)
}


reduce_raw_analysis_data <- function(settings, reducer, scan) {

    raw_analysis_filename_glob <- str_c("*",
                                        data_table_extension(settings$binary,
                                                             settings$compression_level))

    read_raw_analysis_data <- function(input_dirpath,
                                       begin_input_filepath,
                                       finish_input_filepath) {

        analyses <- new.env(parent = emptyenv(), hash = TRUE)

        input_dirpath %>%
            dir_ls(type = "file", recursive = FALSE, glob = raw_analysis_filename_glob) %>%
            map(function(raw_data_filepath) {
                filename <- path_ext_remove(path_ext_remove(path_file(raw_data_filepath)))
                delayedAssign(filename,
                              read_data_table(path(settings$input_dirpath, filename),
                                              binary = settings$binary,
                                              compression_level = settings$compression_level),
                              assign.env = analyses)
            })

        analyses$BEGIN <- read_lines(begin_input_filepath)

        analyses$FINISH <- read_lines(finish_input_filepath)

        analyses
    }

    write_raw_analysis_data <- function(analyses, output_dirpath) {
        analyses %>%
            imap(function(table, table_name) {
                filepath <- path(output_dirpath, table_name)
                write_data_table(table,
                                 filepath,
                                 truncate = TRUE,
                                 binary = settings$binary,
                                 compression_level = settings$compression_level)
                filepath
            })
    }

    info("=> Reducing ", settings$input_dirpath, "\n")

    begin_input_filepath <- path(settings$input_dirpath, "BEGIN")
    finish_input_filepath <- path(settings$input_dirpath, "FINISH")
    noerror_input_filepath <- path(settings$input_dirpath, "NOERROR")

    begin_output_filepath <- path(settings$output_dirpath, "BEGIN")
    finish_output_filepath <- path(settings$output_dirpath, "FINISH")
    error_output_filepath <- path(settings$output_dirpath, "ERROR")
    noerror_output_filepath <- path(settings$output_dirpath, "NOERROR")

    dir_create(settings$output_dirpath)

    file_delete(dir_ls(settings$output_dirpath,
                       recursive = FALSE,
                       type = "file"))

    write_file("", begin_output_filepath)

    tryCatch({

        script_type <- path_file(path_dir(settings$input_dirpath))

        info("=> Script type is ", script_type, "\n")

        valid <- (all(file_exists(c(finish_input_filepath, noerror_input_filepath))) &&
                  script_type %in% settings$script_type)

        if (valid) {

            output_filepaths <-
                read_raw_analysis_data(settings$input_dirpath,
                                       begin_input_filepath,
                                       finish_input_filepath) %>%
                reducer() %>%
                write_raw_analysis_data(settings$output_dirpath)

            write_file("", noerror_output_filepath)

            info("=> Reduced ", settings$input_dirpath, "\n")

            output_filepaths
        }
        else {

            write_file("", error_output_filepath)

            info("=> Invalid ", settings$input_dirpath, "\n")

            ## empty filepaths returned for consistency
            list("")
        }

    },

    error = function(e) {
        write_file("", error_output_filepath)
        info("=> Error reducing ", settings$input_dirpath, "\n")
        stop(e)
    },

    finally = {
        write_file("", finish_output_filepath)
    })
}

parse_program_arguments <- function() {

    usage <- "%prog raw-analysis-dirpath reduced-analysis-dirpath analysis %options"

    description <- paste(
        "raw-analysis-dirpath       directory containing raw data files",
        "reduced-analysis-dirpath   directory to which reduced data will be exported",
        "analysis                   name of analysis to run",
        sep = "\n")


    option_list <- list(
        make_option(c("--vignettes"),
                    action = "store_true",
                    default = FALSE,
                    help = "reduce raw data from vignettes",
                    metavar = "vignettes"),

        make_option(c("--examples"),
                    action = "store_true",
                    default = FALSE,
                    help = "reduce raw data from examples",
                    metavar = "examples"),

        make_option(c("--tests"),
                    action = "store_true",
                    default = FALSE,
                    help = "reduce raw data from tests",
                    metavar = "tests"),

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

    script_type <- c()

    if(arguments$options$vignettes) script_type <- c(script_type, "doc")
    if(arguments$options$examples) script_type <- c(script_type, "examples")
    if(arguments$options$tests) script_type <- c(script_type, "tests")

    if(length(script_type) == 0) {
        stop("script type not specified (--vignettes, --examples, --tests)")
    }

    list(input_dirpath = arguments$args[1],
         output_dirpath = arguments$args[2],
         analysis = arguments$args[3],
         script_type = script_type,
         binary = arguments$options$binary,
         compression_level = as.integer(arguments$options$compression_level))
}


main <- function() {

    settings <- parse_program_arguments()

    print(settings)

    output_dirpaths <- reduce_raw_analysis_data(settings,
                                                eval(as.symbol(settings$analysis)))

    output_dirpaths
}


main()
