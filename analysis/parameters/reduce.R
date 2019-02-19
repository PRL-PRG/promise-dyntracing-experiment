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
    list(object_count_distribution_by_type = analyses$object_count)
}


functions <- function(analyses) {
    ## add graph for distribution of calls between
    ## closures, builtins, specials
    ## and number of closures, builtins and specials

    closure_call_summary <-
        analyses$call_summary %>%
        filter(function_type == "Closure")

    closure_force_order_count <-
        closure_call_summary %>%
        group_by(function_id, wrapper, force_order, missing_arguments) %>%
        summarize(call_count = sum(call_count))

    closure_return_type <-
        closure_call_summary %>%
        group_by(function_id, return_value_type) %>%
        summarize(call_count = sum(call_count))

    closure_parameter_count <-
        closure_call_summary %>%
        group_by(function_id) %>%
        summarize(formal_parameter_count = first(formal_parameter_count))

    list(closure_force_order_count = closure_force_order_count,
         closure_return_type = closure_return_type,
         closure_parameter_count = closure_parameter_count)
}


argument_promise_relation <- function(analyses) {

    non_argument_promise_count <-
        analyses$non_argument_promises %>%
        pull(value_id) %>%
        unique() %>%
        length()

    argument_promises <-
        analyses$arguments %>%
        filter(argument_type == "Promise")

    argument_promise_count <-
        argument_promises %>%
        pull(value_id) %>%
        unique() %>%
        length()

    promise_distribution_by_source <-
        tibble(promise_type = c("Argument", "Non Argument"),
               promise_count = c(argument_promise_count, non_argument_promise_count))


    argument_distribution_by_type <-
        analyses$arguments %>%
        group_by(argument_type) %>%
        summarize(argument_count = n()) %>%
        ungroup()

    argument_distribution_by_dot_dot <-
        analyses$arguments %>%
        group_by(dot_dot) %>%
        summarize(argument_count = n()) %>%
        ungroup()

    argument_promise_distribution_by_default <-
        argument_promises %>%
        group_by(default) %>%
        summarize(argument_count = n()) %>%
        ungroup()

    argument_promise_distribution_by_dot_dot_and_sharing <-
        argument_promises %>%
        group_by(dot_dot, value_id) %>%
        summarize(sharing_count = n()) %>%
        ungroup() %>%
        group_by(dot_dot, sharing_count) %>%
        summarize(promise_count = n()) %>%
        ungroup()

    list(promise_distribution_by_source = promise_distribution_by_source,
         argument_distribution_by_type = argument_distribution_by_type,
         argument_distribution_by_dot_dot = argument_distribution_by_dot_dot,
         argument_promise_distribution_by_default = argument_promise_distribution_by_default,
         argument_promise_distribution_by_dot_dot_and_sharing = argument_promise_distribution_by_dot_dot_and_sharing)
}

## TODO - makefile should output logs in different files.
promise_forcing <- function(analyses) {

    argument_promises <-
        analyses$arguments %>%
        filter(argument_type == "Promise")

    argument_promise_count <-
        argument_promises %>%
        pull(value_id) %>%
        unique() %>%
        length()

    non_local_return_argument_promises <-
        argument_promises %>%
        filter(non_local_return)

    non_local_return_argument_promise_count <-
        non_local_return_argument_promises %>%
        pull(value_id) %>%
        unique() %>%
        length()

    ## TODO - how many calls are jumped when non local return happens?

    list()
}

promise_use_and_force <- function(analyses) {

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

    summarize_force_time <- function(table) {

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

    ############################################################################
    ## Escaped Promises
    ############################################################################

    escaped_argument_promises <-
        analyses$escaped_arguments %>%
        ## this filter is probably redundant because only escaped promises
        ## are tracked. having it here is not going to affect the output
        ## though.
        filter(argument_type == "Promise") %>%
        mutate(before_escape_force = as.logical(before_escape_force_count),
               before_escape_lookup = as.logical(before_escape_value_lookup_count),
               before_escape_metaprogram = as.logical(before_escape_metaprogram_count),
               after_escape_force = as.logical(after_escape_force_count),
               after_escape_lookup = as.logical(after_escape_value_lookup_count),
               after_escape_metaprogram = as.logical(after_escape_metaprogram_count)) %>%
        mutate(before_escape_lookup = before_escape_lookup | before_escape_force | preforce,
               after_escape_lookup = after_escape_lookup | after_escape_force)

    before_escape_argument_promise_use_counts <-
        escaped_argument_promises %>%
        select(lookup = before_escape_lookup,
               metaprogram = before_escape_metaprogram) %>%
        summarize_use() %>%
        add_column(category = "Before Escape Argument", .before = 1)

    after_escape_argument_promise_use_counts <-
        escaped_argument_promises %>%
        select(lookup = after_escape_lookup,
               metaprogram = after_escape_metaprogram) %>%
        summarize_use() %>%
        add_column(category = "After Escape Argument", .before = 1)

    before_escape_argument_promise_force_counts <-
        escaped_argument_promises %>%
        select(preforce,
               force = before_escape_force) %>%
        summarize_force_time() %>%
        add_column(category = "Before Escape Argument", .before = 1)

    after_escape_argument_promise_force_counts <-
        escaped_argument_promises %>%
        select(preforce,
               force = after_escape_force) %>%
        summarize_force_time() %>%
        add_column(category = "After Escape Argument", .before = 1)

    difference_escape_argument_promise_force_counts <-
        escaped_argument_promises %>%
        filter(!before_escape_force & after_escape_force) %>%
        nrow()

    difference_escape_argument_promise_lookup_counts <-
        escaped_argument_promises %>%
        filter(!before_escape_lookup & after_escape_lookup) %>%
        nrow()

    difference_escape_argument_promise_metaprogram_counts <-
        escaped_argument_promises %>%
        filter(!before_escape_metaprogram & after_escape_metaprogram) %>%
        nrow()

    difference_escape_argument_promise_lookup_counts <-
        escaped_argument_promises %>%
        filter(!before_escape_lookup & after_escape_lookup) %>%
        nrow()

    ############################################################################
    ## Argument Promises
    ############################################################################
    argument_promises <-
        analyses$arguments %>%
        filter(argument_type == "Promise") %>%
        mutate(force = as.logical(force_count),
               lookup = as.logical(value_lookup_count),
               metaprogram = as.logical(metaprogram_count)) %>%
        mutate(lookup = lookup | force | preforce) %>%
        ## group by because same promise can occur in multiple
        ## argument positions
        group_by(value_id) %>%
        summarize(force = any(force),
                  lookup = any(lookup),
                  metaprogram = any(metaprogram),
                  preforce = any(preforce)) %>%
        ungroup()

    argument_promise_use_counts <-
        argument_promises %>%
        summarize_use() %>%
        add_column(category = "Argument", .before = 1)

    argument_promise_force_counts <-
        argument_promises %>%
        summarize_force_time() %>%
        add_column(category = "Argument", .before = 1)

    ############################################################################
    ## Non Argument Promises
    ############################################################################
    non_argument_promises <-
        analyses$non_argument_promises %>%
        select(preforce, force_count, value_lookup_count, metaprogram_count) %>%
        mutate(force = as.logical(force_count),
               lookup = as.logical(value_lookup_count),
               metaprogram = as.logical(metaprogram_count)) %>%
        mutate(lookup = lookup | force | preforce)

    non_argument_promise_use_counts <-
        non_argument_promises %>%
        summarize_use() %>%
        add_column(category = "Non Argument", .before = 1)

    non_argument_promise_force_counts <-
        non_argument_promises %>%
        summarize_force_time() %>%
        add_column(category = "Non Argument", .before = 1)

    ############################################################################
    ## Result
    ############################################################################

    promise_count_by_category <- tibble(
        category = c("Argument Promise", "Non Argument Promise"),
        promise_count = c(nrow(argument_promises), nrow(non_argument_promises))
    )

    argument_promise_count_by_category <- tibble(
        category = c("Unescaped Argument Promise", "Escaped Argument Promise"),
        promise_count = c(nrow(argument_promises) - nrow(escaped_argument_promises),
                          nrow(escaped_argument_promises))
    )

    list(promise_count_by_category = promise_count_by_category,
         argument_promise_count_by_category = argument_promise_count_by_category,
         promise_use_counts = bind_rows(argument_promise_use_counts,
                                        non_argument_promise_use_counts),
         promise_force_counts = bind_rows(argument_promise_force_counts,
                                          non_argument_promise_force_counts),
         escaped_promise_use_counts = bind_rows(before_escape_argument_promise_use_counts,
                                                after_escape_argument_promise_use_counts),
         escaped_promise_force_counts = bind_rows(before_escape_argument_promise_force_counts,
                                                  after_escape_argument_promise_force_counts))
}

parameters <- function(analyses) {

    ## TODO - add 1 for non promise arguments for lookup etc.
    formal_parameter_usage_counts <-
        analyses$arguments %>%
        select(call_id, function_id, formal_parameter_position,
               preforce, force_count, value_lookup_count, metaprogram_count) %>%
        mutate(lookup = as.logical(value_lookup_count),
               metaprogram = as.logical(metaprogram_count),
               force = as.logical(force_count)) %>%
        mutate(lookup_use = force | preforce | lookup,
               metaprogram_use = metaprogram,
               use = lookup_use | metaprogram_use) %>%
        select(call_id, function_id, formal_parameter_position, lookup_use, metaprogram_use, use) %>%
        group_by(call_id, formal_parameter_position) %>%
        summarize(function_id = first(function_id),
                  metaprogram_use = any(metaprogram_use),
                  lookup_use = any(lookup_use),
                  use = any(use)) %>%
        ungroup() %>%
        group_by(function_id, formal_parameter_position) %>%
        summarize(lookup_use = sum(lookup_use),
                  metaprogram_use = sum(metaprogram_use),
                  use = sum(use)) %>%
        ungroup()

    list(formal_parameter_usage_counts = formal_parameter_usage_counts)
}


escaped_promise_functions <- function(analyses) {

    escaped_argument_functions <-
        analyses$escaped_arguments %>%
        group_by(call_id) %>%
        mutate(category = if_else(n() == formal_parameter_count, "All", "Some")) %>%
        ungroup() %>%
        select(-call_id)

    escaped_argument_call_return_value_type <-
        analyses$escaped_arguments %>%
        group_by(return_value_type) %>%
        summarize(count = length(unique(call_id)))

    list(escaped_argument_functions = escaped_argument_functions,
         escaped_argument_call_return_value_type = escaped_argument_call_return_value_type)
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
                 raw_data_dirpath, valid, raw_analysis_filename) {

            info("=> Reducing ", scriptname, " from ", package, "\n")
            raw_analysis_filename <- raw_analysis_filename[[1]]
            reduced_analysis_dirpath <- path(settings$output_dirpath,
                                             script_type,
                                             scriptname)
            dir_create(reduced_analysis_dirpath)

            output_filepaths <-
                path(settings$input_dirpath,
                     script_type,
                     scriptname,
                     raw_analysis_filename) %>%
                map(reader) %>%
                setNames(path_ext_remove(raw_analysis_filename)) %>%
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
               valid = map_lgl(script_dirpaths, script_is_valid),
               raw_analysis_filename = list(list(settings$table_names)))

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

    analysis_map <- list(
        object_type = list("object_count"),
        functions = list("call_summary"),
        argument_promise_relation = list("arguments", "non_argument_promises"),
        promise_use_and_force = list("arguments", "non_argument_promises", "escaped_arguments"),
        parameters = list("arguments", "call_summary"),
        functions = list("call_summary"),
        escaped_promises = list("escaped_arguments")
    )

    list(input_dirpath = arguments$args[1],
         output_dirpath = arguments$args[2],
         analysis = arguments$args[3],
         table_names = analysis_map[[arguments$args[3]]],
         script_type = script_type,
         binary = arguments$options$binary,
         compression_level = arguments$options$compression_level)
}


main <- function() {
    settings <- parse_program_arguments()
    print(settings)
    script_table <- scan_input_dirpath(settings)
    reduce_raw_analysis_data(settings, script_table)
}


main()
