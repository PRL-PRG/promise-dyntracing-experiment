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

info <- function(...) cat((paste0(...)))

combine_analyses <- function(datasets, combiner = dplyr::bind_rows) {

    for(name in names(datasets)) {
        pb$tick(tokens = list(what = name))
        datasets[[name]] = combiner(datasets[[name]])
    }
    datasets
}


combine_reduced_data <- function(settings, reduced_data_table) {

    read_data_file_and_mutate <- function(package, script_type,
                                          script_name, data_filepath) {
        promisedyntracer::read_data_table(path_ext_remove(path_ext_remove(data_filepath)),
                                          binary = settings$binary,
                                          compression_level = settings$compression_level) %>%
            mutate(package = package,
                   script_type = script_type,
                   script_name = script_name)
    }

    reduced_data_table %>%
        group_by(data_filename) %>%
        do({
            info("=> Reading reduced data files ", .data$data_filename[1], "\n")

            output_filepath <- path(settings$output_dirpath,
                                    path_ext_remove(path_ext_remove(.data$data_filename[1])))

            .data %>%
                mutate(data_filepath = path(dirpath, data_filename)) %>%
                select(package, script_type, script_name, data_filepath) %>%
                pmap(read_data_file_and_mutate) %>%
                bind_rows() %>%
                promisedyntracer::write_data_table(output_filepath,
                                                   binary = settings$binary,
                                                   compression_level = settings$compression_level)

            info("=> Writing combined data file to ", output_filepath, "\n")
            tibble(output_filepath)
        }) %>%
        ungroup()
}


scan_input_dirpath <- function(settings) {

    info("=> Scanning for reduced data files in ", settings$input_dirpath, "\n")

    ext <- promisedyntracer::data_table_extension(settings$binary,
                                                  settings$compression_level)
    glob <- paste0("*", ext)

    reduced_script_data_dirpaths <-
        settings$input_dirpath %>%
        dir_ls() %>%
        map(function(package_dirpath) path(package_dirpath, settings$script_type)) %>%
        flatten_chr() %>%
        purrr::keep(dir_exists) %>%
        dir_ls()

    reduced_script_data_filenames <-
        reduced_script_data_dirpaths %>%
        map(dir_ls, type = "file", glob = glob) %>%
        map(path_file)

    reduced_data_table <-
        tibble(dirpath = reduced_script_data_dirpaths,
               data_filename = reduced_script_data_filenames) %>%
        unnest(data_filename) %>%
        mutate(package = path_file(path_dir(path_dir(dirpath))),
               script_type = path_file(path_dir(dirpath)),
               script_name = path_file(dirpath))

    info("=> Found ", nrow(reduced_data_table), " data files\n")

    reduced_data_table
}


parse_program_arguments <- function() {

    usage <- "%prog reduced-output-dirpath combined-output-dirpath"
    description <- paste(
        "reduced-output-dirpath    directory containing reduced data files (scanned recursively)",
        "combined-output-dirpath   directory to which combined data will be exported",
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
