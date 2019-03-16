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
suppressPackageStartupMessages(library(progress))
suppressPackageStartupMessages(library(tibble))
suppressPackageStartupMessages(library(readr))

info <- function(...) cat((paste0(...)))


scan_input_dirpath <- function(settings) {

    info("=> Scanning '", settings$input_dirpath, "'.\n")

    reduced_analyses <-
        settings$input_dirpath %>%
        dir_ls(type = "directory")

    ## we use this to ensure that for any program,
    ## these many FINISH files exist in the reduced
    ## data directory
    analyses_count <- length(reduced_analyses)

    all_scripts <-
        settings$input_dirpath %>%
        ## analyses
        dir_ls(type = "directory") %>%
        ## package names
        dir_ls(type = "directory") %>%
        ## script types
        dir_ls(type = "directory") %>%
        ## script names
        dir_ls(type = "directory") %>%
        {tibble(script_dirpath = .)} %>%
        mutate(script_name = path_file(script_dirpath),
               script_type = path_file(path_dir(script_dirpath)),
               package = path_file(path_dir(path_dir(script_dirpath))),
               analysis = path_file(path_dir(path_dir(path_dir(script_dirpath)))),
               finish_filepath = path(script_dirpath, "FINISH"),
               noerror_filepath = path(script_dirpath, "NOERROR")) %>%
        mutate(finish_and_noerror = file_exists(finish_filepath) & file_exists(noerror_filepath)) %>%
        group_by(package, script_type, script_name) %>%
        summarize(analyses = str_c("(", str_c(analysis, collapse = " "), ")"),
                  finish_and_noerror = all(finish_and_noerror),
                  all_analyses = length(analysis) == analyses_count) %>%
        ungroup() %>%
        mutate(valid_script_type = script_type %in% settings$script_types) %>%
        mutate(valid = valid_script_type & finish_and_noerror & all_analyses)

    info("Found ", sum(all_scripts$valid), " valid programs.\n")

    info("Found ", sum(!all_scripts$valid), " invalid programs.\n")

    info("Writing all scripts to '",  settings$all_scripts_filepath, "'.\n")

    all_scripts %>%
        write_csv(path(settings$all_scripts_filepath))

    info("Writing valid scripts to '",  settings$valid_scripts_filepath, "'.\n")

    all_scripts %>%
        filter(valid) %>%
        mutate(script_dirpath = path(package, script_type, script_name)) %>%
        select(script_dirpath) %>%
        write_csv(path(settings$valid_scripts_filepath), col_names = FALSE)

    info("Writing invalid scripts to '",  settings$valid_scripts_filepath, "'.\n")

    all_scripts %>%
        filter(!valid) %>%
        write_csv(path(settings$invalid_scripts_filepath))

    info("=> Finish scanning '", settings$input_dirpath, "'.\n")

    all_scripts
}


parse_program_arguments <- function() {

    usage <- "%prog reduced-output-dirpath combined-output-dirpath analysis combine-count"
    description <- paste(
        "reduced-output-dirpath    directory containing reduced data files (scanned recursively)",
        "all-scripts-filepath      file to which information about all scripts will be exported",
        "valid-scripts-filepath    file to which information about valid scripts will be exported",
        "invalid-scripts-filepath  file to which information about invalid scripts will be exported",
        sep = "\n")


    option_list <- list(
        make_option(c("--vignettes"),
                    action="store_true",
                    default=FALSE,
                    help="combine reduced data from vignettes",
                    metavar="vignettes"),

        make_option(c("--examples"),
                    action="store_true",
                    default=FALSE,
                    help="combine reduced data from examples",
                    metavar="examples"),

        make_option(c("--tests"),
                    action="store_true",
                    default=FALSE,
                    help="combine reduced data from tests",
                    metavar="tests")
    )


    option_parser <- OptionParser(usage = usage,
                                  description = description,
                                  add_help_option = TRUE,
                                  option_list = option_list)

    arguments <- parse_args2(option_parser)

    script_types <- c()

    if(arguments$options$vignettes) script_types <- c(script_types, "doc")
    if(arguments$options$examples) script_types <- c(script_types, "examples")
    if(arguments$options$tests) script_types <- c(script_types, "tests")

    if(length(script_types) == 0) {
        stop("script type not specified (--vignettes, --examples, --tests)")
    }

    list(input_dirpath = arguments$args[1],
         all_scripts_filepath = arguments$args[2],
         valid_scripts_filepath = arguments$args[3],
         invalid_scripts_filepath = arguments$args[4],
         script_types = script_types)
}


main <- function() {
    settings <- parse_program_arguments()

    print(settings)

    scan_input_dirpath(settings)

    invisible(NULL)
}


main()
