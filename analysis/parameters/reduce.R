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


source("analysis/utils.R")
source("analysis/analysis.R")


info <- function(...) cat((paste0(...)))


object_type <- function(analyses) {
    ## object type count is already summarized by the tracer.
    ## we only have to emit the same file again for summarization.
    list(object_counts = analyses$object_counts)
}


functions <- function(analyses) {

    function_call_summary <-
        analyses$call_summaries %>%
        group_by(function_type, function_id,
                 S3_method, S4_method,
                 jumped, return_value_type) %>%
        summarize(call_count = sum(as.double(call_count))) %>%
        ungroup()

    closure_call_summary <-
        analyses$call_summaries %>%
        filter(function_type == "Closure")

    ## only take into account closures that are not jumped
    closure_force_order_count <-
        closure_call_summary %>%
        filter(!jumped) %>%
        group_by(function_id, wrapper, force_order, missing_arguments) %>%
        summarize(formal_parameter_count = first(formal_parameter_count),
                  call_count = sum(as.double(call_count))) %>%
        ungroup()

    closure_parameter_count <-
        closure_call_summary %>%
        group_by(function_id) %>%
        summarize(formal_parameter_count = first(formal_parameter_count)) %>%
        ungroup()

    list(function_call_summary = function_call_summary,
         closure_force_order_count = closure_force_order_count,
         closure_parameter_count = closure_parameter_count)
}


arguments <- function(analyses) {

    argument_promises <-
        analyses$arguments %>%
        filter(argument_type == "Promise")

    argument_distribution_by_type <-
        analyses$arguments %>%
        group_by(argument_type) %>%
        summarize(argument_count = n()) %>%
        ungroup()

    argument_distribution_by_dot_dot_dot <-
        analyses$arguments %>%
        group_by(dot_dot_dot) %>%
        summarize(argument_count = n()) %>%
        ungroup() %>%
        mutate(argument_category = c("Standard", "...")[dot_dot_dot + 1]) %>%
        select(argument_category, argument_count)

    argument_promise_distribution_by_nature <-
        argument_promises %>%
        group_by(default) %>%
        summarize(argument_count = n()) %>%
        ungroup() %>%
        mutate(argument_nature = c("Non Default", "Default")[default + 1]) %>%
        select(argument_nature, argument_count)

    argument_promise_distribution_by_sharing <-
        argument_promises %>%
        group_by(value_id) %>%
        summarize(sharing_count = n()) %>%
        ungroup() %>%
        group_by(sharing_count) %>%
        summarize(promise_count = n()) %>%
        ungroup()

    list(argument_distribution_by_type = argument_distribution_by_type,
         argument_distribution_by_dot_dot_dot = argument_distribution_by_dot_dot_dot,
         argument_promise_distribution_by_nature = argument_promise_distribution_by_nature,
         argument_promise_distribution_by_sharing = argument_promise_distribution_by_sharing)
}


parameters <- function(analyses) {

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
        group_by(function_id, formal_parameter_position, call_id) %>%
        summarize(metaprogram = any(metaprogram),
                  lookup = any(lookup)) %>%
        mutate(either = metaprogram | lookup,
               both = metaprogram & lookup) %>%
        summarize(lookup = sum(lookup),
                  metaprogram = sum(metaprogram),
                  either = sum(either),
                  both = sum(both),
                  call_count = n()) %>%
        ungroup()

    execution_times <-
        analyses$arguments %>%
        select(function_id, formal_parameter_position, direct_force, execution_time) %>%
        mutate(execution_time = execution_time / 1000000) %>%
        filter(direct_force != 0 & execution_time >= 1) %>%
        group_by(function_id, formal_parameter_position, execution_time) %>%
        summarize(argument_count = 1.0 * n())

    list(formal_parameter_usage_counts = formal_parameter_usage_counts,
         execution_times = execution_times)
}


escaped_arguments <- function(analyses) {

    list(escaped_arguments = analyses$escaped_arguments)
}


promises <- function(analyses) {
    ## TODO
    ## "call_depth",
    ## "promise_depth",
    ## "nested_promise_depth",

    ## TODO - makefile should output logs in different files.
    ## promise_forcing <- function(analyses) {
    ##     argument_promises <-
    ##         analyses$promises %>%
    ##         filter(argument)

    ##     argument_promise_count <-
    ##         argument_promises %>%
    ##         pull(value_id) %>%
    ##         unique() %>%
    ##         length()

    ##     non_local_return_argument_promises <-
    ##         argument_promises %>%
    ##         filter(non_local_return)

    ##     non_local_return_argument_promise_count <-
    ##         non_local_return_argument_promises %>%
    ##         pull(value_id) %>%
    ##         unique() %>%
    ##         length()

    ##     ## TODO - how many calls are jumped when non local return happens?

    ##     list()
    ## }

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
        group_by(expression_type) %>%
        summarize(promise_count = n()) %>%
        ungroup()

    non_argument_promise_count_by_expression_type <-
        non_argument_promises %>%
        group_by(expression_type) %>%
        summarize(promise_count = n()) %>%
        ungroup()

    argument_promise_count_by_value_type <-
        argument_promises %>%
        group_by(value_type) %>%
        summarize(promise_count = n()) %>%
        ungroup()

    non_argument_promise_count_by_value_type <-
        non_argument_promises %>%
        group_by(value_type) %>%
        summarize(promise_count = n()) %>%
        ungroup()

    argument_promise_count_by_creation_scope <-
        argument_promises %>%
        group_by(creation_scope) %>%
        summarize(promise_count = n()) %>%
        ungroup()

    non_argument_promise_count_by_creation_scope <-
        non_argument_promises %>%
        group_by(creation_scope) %>%
        summarize(promise_count = n()) %>%
        ungroup()

    argument_promise_count_by_forcing_scope <-
        argument_promises %>%
        group_by(forcing_scope) %>%
        summarize(promise_count = n()) %>%
        ungroup()

    non_argument_promise_count_by_forcing_scope <-
        non_argument_promises %>%
        group_by(forcing_scope) %>%
        summarize(promise_count = n()) %>%
        ungroup()

    argument_promise_count_by_dispatch_type <-
        argument_promises %>%
        select(S3_dispatch, S4_dispatch) %>%
        mutate(dispatch_type =
                   if_else(!S3_dispatch & !S4_dispatch, "No Dispatch",
                           if_else(S3_dispatch & !S4_dispatch, "S3 Dispatch",
                                   if_else(!S3_dispatch & S4_dispatch, "S4 Dispatch",
                                           "Both")))) %>%
        group_by(dispatch_type) %>%
        summarize(promise_count = n()) %>%
        ungroup()

    summarize_event_counts <- function(df, group_column) {
        group_column <- enquo(group_column)

        df %>%
            group_by(!!group_column) %>%
            summarize(promise_count = n()) %>%
            ungroup()
    }

    promise_count_by_force_count <-
        argument_promises %>%
        summarize_event_counts(force_count)

    promise_count_by_metaprogram_count <-
        argument_promises %>%
        summarize_event_counts(metaprogram_count)

    promise_count_by_value_lookup_count <-
        argument_promises %>%
        summarize_event_counts(value_lookup_count)

    promise_count_by_value_assign_count <-
        argument_promises %>%
        summarize_event_counts(value_assign_count)

    promise_count_by_expression_lookup_count <-
        argument_promises %>%
        summarize_event_counts(expression_lookup_count)

    promise_count_by_expression_assign_count <-
        argument_promises %>%
        summarize_event_counts(expression_assign_count)

    promise_count_by_environment_lookup_count <-
        argument_promises %>%
        summarize_event_counts(environment_lookup_count)

    promise_count_by_environment_assign_count <-
        argument_promises %>%
        summarize_event_counts(environment_assign_count)


    summarize_side_effect <- function(df, side_effect_column, type_column) {
        side_effect_column <- enquo(side_effect_column)

        total_argument_promise_count <-
            nrow(df)

        side_effect_promise_count <-
            df %>%
            pull(!!side_effect_column) %>%
            as.logical() %>%
            sum()

        no_side_effect_promise_count <-
            total_argument_promise_count - side_effect_promise_count

        tibble(c("Yes", "No"),
               c(side_effect_promise_count, no_side_effect_promise_count)) %>%
            `colnames<-`(c(type_column, "promise_count"))
    }

    promise_count_by_direct_self_scope_mutation <-
        argument_promises %>%
        summarize_side_effect(direct_self_scope_mutation_count,
                              "direct_self_scope_mutation")

    promise_count_by_indirect_self_scope_mutation <-
        argument_promises %>%
        summarize_side_effect(indirect_self_scope_mutation_count,
                              "indirect_self_scope_mutation")

    promise_count_by_direct_lexical_scope_mutation <-
        argument_promises %>%
        summarize_side_effect(direct_lexical_scope_mutation_count,
                              "direct_lexical_scope_mutation")

    promise_count_by_indirect_lexical_scope_mutation <-
        argument_promises %>%
        summarize_side_effect(indirect_lexical_scope_mutation_count,
                              "indirect_lexical_scope_mutation")

    promise_count_by_direct_non_lexical_scope_mutation <-
        argument_promises %>%
        summarize_side_effect(direct_non_lexical_scope_mutation_count,
                              "direct_non_lexical_scope_mutation")

    promise_count_by_indirect_non_lexical_scope_mutation <-
        argument_promises %>%
        summarize_side_effect(indirect_non_lexical_scope_mutation_count,
                              "indirect_non_lexical_scope_mutation")

    promise_count_by_direct_self_scope_observation <-
        argument_promises %>%
        summarize_side_effect(direct_self_scope_observation_count,
                              "direct_self_scope_observation")

    promise_count_by_indirect_self_scope_observation <-
        argument_promises %>%
        summarize_side_effect(indirect_self_scope_observation_count,
                              "indirect_self_scope_observation")

    promise_count_by_direct_lexical_scope_observation <-
        argument_promises %>%
        summarize_side_effect(direct_lexical_scope_observation_count,
                              "direct_lexical_scope_observation")

    promise_count_by_indirect_lexical_scope_observation <-
        argument_promises %>%
        summarize_side_effect(indirect_lexical_scope_observation_count,
                              "indirect_lexical_scope_observation")

    promise_count_by_direct_non_lexical_scope_observation <-
        argument_promises %>%
        summarize_side_effect(direct_non_lexical_scope_observation_count,
                              "direct_non_lexical_scope_observation")

    promise_count_by_indirect_non_lexical_scope_observation <-
        argument_promises %>%
        summarize_side_effect(indirect_non_lexical_scope_observation_count,
                              "indirect_non_lexical_scope_observation")

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

    promises <-
        analyses$promises %>%
        mutate(force = as.logical(force_count),
               lookup = as.logical(value_lookup_count),
               metaprogram = as.logical(metaprogram_count)) %>%
        mutate(lookup = force | lookup) %>%
        select(promise_category = argument, force, lookup, metaprogram, preforce) %>%
        mutate(promise_category = c("Non Argument", "Argument")[promise_category + 1])

    promise_use_distribution_by_category <-
        promises %>%
        group_by(promise_category) %>%
        do(summarize_use(.data)) %>%
        ungroup()

    promise_action_distribution_by_category <-
        promises %>%
        group_by(promise_category) %>%
        do(summarize_action(.data)) %>%
        ungroup()

    list(promise_count_by_category = promise_count_by_category,
         argument_promise_count_by_expression_type = argument_promise_count_by_expression_type,
         non_argument_promise_count_by_expression_type = non_argument_promise_count_by_expression_type,
         argument_promise_count_by_value_type = argument_promise_count_by_value_type,
         non_argument_promise_count_by_value_type = non_argument_promise_count_by_value_type,
         argument_promise_count_by_creation_scope = argument_promise_count_by_creation_scope,
         non_argument_promise_count_by_creation_scope = non_argument_promise_count_by_creation_scope,
         argument_promise_count_by_forcing_scope = argument_promise_count_by_forcing_scope,
         non_argument_promise_count_by_forcing_scope = non_argument_promise_count_by_forcing_scope,
         argument_promise_count_by_dispatch_type = argument_promise_count_by_dispatch_type,
         promise_count_by_force_count = promise_count_by_force_count,
         promise_count_by_metaprogram_count = promise_count_by_metaprogram_count,
         promise_count_by_value_lookup_count = promise_count_by_value_lookup_count,
         promise_count_by_value_assign_count = promise_count_by_value_assign_count,
         promise_count_by_expression_lookup_count = promise_count_by_expression_lookup_count,
         promise_count_by_expression_assign_count = promise_count_by_expression_assign_count,
         promise_count_by_environment_lookup_count = promise_count_by_environment_lookup_count,
         promise_count_by_environment_assign_count = promise_count_by_environment_assign_count,
         promise_count_by_direct_self_scope_mutation = promise_count_by_direct_self_scope_mutation,
         promise_count_by_indirect_self_scope_mutation = promise_count_by_indirect_self_scope_mutation,
         promise_count_by_direct_lexical_scope_mutation = promise_count_by_direct_lexical_scope_mutation,
         promise_count_by_indirect_lexical_scope_mutation = promise_count_by_indirect_lexical_scope_mutation,
         promise_count_by_direct_non_lexical_scope_mutation = promise_count_by_direct_non_lexical_scope_mutation,
         promise_count_by_indirect_non_lexical_scope_mutation = promise_count_by_indirect_non_lexical_scope_mutation,
         promise_count_by_direct_self_scope_observation = promise_count_by_direct_self_scope_observation,
         promise_count_by_indirect_self_scope_observation = promise_count_by_indirect_self_scope_observation,
         promise_count_by_direct_lexical_scope_observation = promise_count_by_direct_lexical_scope_observation,
         promise_count_by_indirect_lexical_scope_observation = promise_count_by_indirect_lexical_scope_observation,
         promise_count_by_direct_non_lexical_scope_observation = promise_count_by_direct_non_lexical_scope_observation,
         promise_count_by_indirect_non_lexical_scope_observation = promise_count_by_indirect_non_lexical_scope_observation,
         promise_use_distribution_by_category = promise_use_distribution_by_category,
         promise_action_distribution_by_category = promise_action_distribution_by_category)
}


function_definitions <- function(analyses) {
    list(function_definitions = analyses$function_definitions)
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


reduce_raw_analysis_data <- function(settings, script_table) {

    reducer <- eval(as.symbol(settings$analysis))

    reader <- function(filepath)
        promisedyntracer::read_data_table(filepath,
                                          binary = settings$binary,
                                          compression_level = settings$compression_level)

    reduce_raw_analysis_datum <-
        function(package, script_type, scriptname, dirpath,
                 raw_data_dirpath, valid) {

            info("=> Reducing ", scriptname, " from ", package, "\n")

            reduced_analysis_dirpath <- path(settings$output_dirpath,
                                             script_type,
                                             scriptname)

            dir_create(reduced_analysis_dirpath)

            analyses <- new.env(parent = emptyenv(), hash = TRUE)

            path(settings$input_dirpath,
                 script_type,
                 scriptname) %>%
                dir_ls() %>%
                map(function(raw_data_filepath) {
                    name <- path_ext_remove(path_ext_remove(path_file(raw_data_filepath)))

                    if (name %in% c("BEGIN", "NOERROR", "FINISH", "ERROR")) {
                        NULL
                    }
                    else {
                        delayedAssign(name,
                                      read_data_table(path_ext_remove(path_ext_remove(raw_data_filepath)),
                                                      binary = settings$binary,
                                                      compression_level = settings$compression_level),
                                      assign.env = analyses)
                    }
                })

            output_filepaths <-
                analyses %>%
                reducer() %>%
                imap(function(table, table_name) {
                    filepath <- path(reduced_analysis_dirpath,
                                     table_name)
                    promisedyntracer::write_data_table(table,
                                                       filepath,
                                                       truncate = TRUE,
                                                       binary = settings$binary,
                                                       compression_level = settings$compression_level)

                    filepath
                })

            info("=> Reduced ", scriptname, " from ", package, "\n")

            output_filepaths
        }

    script_table %>%
        filter(valid) %>%
        pmap(reduce_raw_analysis_datum)
}


scan_input_dirpath <- function(settings) {

    script_is_valid <- function(script_dirpath) {
        all(file_exists(path(script_dirpath, c("NOERROR", "FINISH"))))
    }

    info("=> Scanning for raw data files in ", settings$input_dirpath, "\n")

    script_dirpaths <-
        settings$input_dirpath %>%
        path(settings$script_type) %>%
        purrr::keep(dir_exists) %>%
        dir_ls(type = "directory")

    script_table <-
        tibble(package = path_file(settings$input_dirpath),
               script_type = path_file(path_dir(script_dirpaths)),
               scriptname = path_file(script_dirpaths),
               dirpath = script_dirpaths,
               valid = map_lgl(script_dirpaths, script_is_valid))

    info("=> Found ", nrow(script_table), " scripts, ",
         sum(pull(script_table, valid)), " valid.\n")

    script_table
}



parse_program_arguments <- function() {

    usage <- "%prog raw-package-analysis-dir reduce-package-analysis-dir"
    description <- paste(
        "raw-package-analysis-dir       directory containing raw data files (scanned recursively)",
        "reduce-package-analysis-dir    directory to which reduced data will be exported",
        "analysis                       name of analysis to run",
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
    script_table <- scan_input_dirpath(settings)
    reduce_raw_analysis_data(settings, script_table)
}


main()
