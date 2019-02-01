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

    list(parameters = parameters,
         closures = closures,
         argument_execution_time = argument_execution_time)
}


reduce_raw_analysis_data <- function(settings, script_table) {

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
                     raw_analysis_filename,
                     ext = "csv") %>%
                map(promisedyntracer::read_data_table) %>%
                setNames(path_ext_remove(raw_analysis_filename)) %>%
                reduce_analysis() %>%
                imap(function(table, table_name) {
                    filepath <- path(reduced_analysis_dirpath,
                                     table_name)
                    promisedyntracer::write_data_table(table,
                                                       filepath,
                                                       truncate = TRUE,
                                                       binary = FALSE,
                                                       compression_level = 0)

                    filepath
                })

            info("=> Reduced ", scriptname, " from ", package, "\n")

            output_filepaths
        }

    script_table %>%
        filter(valid) %>%
        pmap(reduce_raw_analysis_datum)
}


scan_input_dirpath <- function(settings, table_names) {

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
               raw_analysis_filename = list(list(table_names)))

    info("=> Found ", nrow(script_table), " scripts\n")

    script_table
}



parse_program_arguments <- function() {

    usage <- "%prog raw-package-analysis-dir reduce-package-analysis-dir"
    description <- paste(
        "raw-package-analysis-dir       directory containing raw data files (scanned recursively)",
        "reduce-package-analysis-dir    directory to which reduced data will be exported",
        sep = "\n")


    option_list <- list(
        make_option(c("--vignettes"),
                    action="store_true",
                    default=FALSE,
                    help="reduce raw data from vignettes",
                    metavar="vignettes"),

        make_option(c("--examples"),
                    action="store_true",
                    default=FALSE,
                    help="reduce raw data from examples",
                    metavar="examples"),

        make_option(c("--tests"),
                    action="store_true",
                    default=FALSE,
                    help="reduce raw data from tests",
                    metavar="tests")
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
         script_type = script_type)
}


main <- function() {
    settings <- parse_program_arguments()
    print(settings)
    table_names <- c("calls", "call-graph", "arguments", "function-callers")
    script_table <- scan_input_dirpath(settings, table_names)
    reduce_raw_analysis_data(settings, script_table)
}


main()
