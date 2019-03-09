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

info <- function(...) cat((paste0(...)))


combine_reduced_data <- function(settings, reduced_data_table) {

    read_data_file_and_mutate <- function(package, script_type,
                                          script_name, data_filepath,
                                          pb) {
        table <-
            data_filepath %>%
            path_ext_remove() %>%
            path_ext_remove() %>%
            read_data_table(binary = settings$binary,
                            compression_level = settings$compression_level)

        if(nrow(table) != 0) {
            table <-
                table %>%
                mutate(package = package,
                       script_type = script_type,
                       script_name = script_name)
        }

        pb$tick(tokens = list(package = package,
                              script_type = script_type,
                              script_name = script_name))
        table
    }

    reduced_data_table %>%
        group_by(data_filename) %>%
        do({
            info("=> Reading reduced data files '", .data$data_filename[1], "'\n")

            pb <- progress_bar$new(format = ":package/:script_type/:script_name [:bar] :percent :eta",
                                   total = nrow(.data),
                                   clear = FALSE,
                                   width = 100)

            output_filepath <- path(settings$output_dirpath,
                                    path_ext_remove(path_ext_remove(.data$data_filename[1])))

            .data %>%
                select(package, script_type, script_name, data_filepath) %>%
                pmap(read_data_file_and_mutate, pb) %>%
                discard(function(df) nrow(df) == 0) %>%
                bind_rows() %>%
                promisedyntracer::write_data_table(output_filepath,
                                                   truncate = TRUE,
                                                   binary = settings$binary,
                                                   compression_level = settings$compression_level)

            info("=> Writing combined data file to ", output_filepath, "\n")

            gc()

            tibble(output_filepath)
        }) %>%
        ungroup()
}


scan_input_dirpath <- function(settings) {

    info("=> Scanning for reduced data files in ", settings$input_dirpath, "\n")

    reduced_analyses <-
        settings$input_dirpath %>%
        dir_ls(type = "directory")

    ## we use this to ensure that for any program,
    ## these many FINISH files exist in the reduced
    ## data directory
    analyses_count <- length(reduced_analyses)

    nth <- function(n) {
        function(components) components[n]
    }

    reduced_scripts <-
        settings$input_dirpath %>%
        dir_ls(recursive = TRUE, type = "file", glob = "*FINISH") %>%
        {tibble(finish_filepaths = .)} %>%
        mutate(relative_finish_filepaths = path_rel(finish_filepaths,
                                                    settings$input_dirpath)) %>%
        mutate(path_components = path_split(relative_finish_filepaths)) %>%
        mutate(analysis = map_chr(path_components, nth(1)),
               package = map_chr(path_components, nth(2)),
               script_type = map_chr(path_components, nth(3)),
               script_name = map_chr(path_components, nth(4))) %>%
        group_by(package, script_type, script_name) %>%
        mutate(finish_count = n()) %>%
        ungroup() %>%
        select(analysis, package, script_type, script_name, finish_count) %>%
        mutate(reduced_data_dirpath = path(settings$input_dirpath,
                                           analysis,
                                           package,
                                           script_type,
                                           script_name)) %>%
        filter(script_type %in% settings$script_type)

    invalid_reduced_scripts <-
        reduced_scripts %>%
        filter(finish_count != analyses_count)

    info("Found ", nrow(invalid_reduced_scripts), " invalid scripts.")

    print(invalid_reduced_scripts, n = Inf)

    valid_reduced_scripts <-
        reduced_scripts %>%
        filter(finish_count == analyses_count) %>%
        filter(analysis == settings$analysis)

    ext <- promisedyntracer::data_table_extension(settings$binary,
                                                  settings$compression_level)
    glob <- paste0("*", ext)

    reduced_script_data_filenames <-
        valid_reduced_scripts %>%
        pull(reduced_data_dirpath) %>%
        map(dir_ls, type = "file", glob = glob) %>%
        map(path_file)

    reduced_data_table <-
        valid_reduced_scripts %>%
        mutate(data_filename = reduced_script_data_filenames) %>%
        unnest(data_filename) %>%
        mutate(data_filepath = path(reduced_data_dirpath, data_filename)) %>%
        select(-finish_count, -reduced_data_dirpath)

    info("=> Found ", nrow(reduced_data_table), " valid data files\n")

    reduced_data_table
}


parse_program_arguments <- function() {

    usage <- "%prog reduced-output-dirpath combined-output-dirpath"
    description <- paste(
        "reduced-output-dirpath    directory containing reduced data files (scanned recursively)",
        "combined-output-dirpath   directory to which combined data will be exported",
        "analysis                  name of analysis to run",
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
                    metavar="tests"),

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
         compression_level = arguments$options$compression_level)
}


main <- function() {
    settings <- parse_program_arguments()
    dir_create(settings$output_dirpath)
    print(settings)
    reduced_data_table <- scan_input_dirpath(settings)
    print(reduced_data_table)
    print(combine_reduced_data(settings, reduced_data_table))
}


main()
