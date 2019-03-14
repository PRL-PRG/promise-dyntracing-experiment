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


read_data_file_and_mutate <- function(sequence_number, filepath, filename,
                                      script_name, script_type, package,
                                      settings, pb) {

    pb$tick(tokens = list(package = package,
                          script_type = script_type,
                          script_name = script_name))

    table <- read_data_table(filepath,
                             binary = settings$binary,
                             compression_level = settings$compression_level)

    if(nrow(table) != 0) {
        table <-
            table %>%
            mutate(package = package,
                   script_type = script_type,
                   script_name = script_name)
    }

    table
}


combine_group <- function(data, settings, output_dirpath) {

    sequence_number <- data$sequence_number[1]

    output_filepath <- path(output_dirpath,
                            sprintf("%s-%06d",
                                    settings$combined_filename_prefix,
                                    sequence_number))

    pb <- progress_bar$new(format = ":package/:script_type/:script_name [:bar] :percent :eta",
                           total = nrow(data),
                           clear = FALSE,
                           width = 100)

    info("=> Combining group ", sequence_number, " to '", output_filepath, "' \n")

    data %>%
        pmap(read_data_file_and_mutate, settings, pb) %>%
        discard(function(df) is.null(df) || nrow(df) == 0 || ncol(df) == 0) %>%
        bind_rows() %>%
        promisedyntracer::write_data_table(output_filepath,
                                           truncate = TRUE,
                                           binary = settings$binary,
                                           compression_level = settings$compression_level)


    tibble(output_filepath = output_filepath)
}


combine_groups <- function(settings, data) {

    filename <- data$filename[1]

    output_dirpath <- path(settings$output_dirpath, settings$analysis, filename)

    dir_create(output_dirpath)

    file_delete(dir_ls(output_dirpath, recursive = FALSE, type = "file"))

    begin_filepath <- path(output_dirpath, "BEGIN")
    finish_filepath <- path(output_dirpath, "FINISH")
    error_filepath <- path(output_dirpath, "ERROR")
    noerror_filepath <- path(output_dirpath, "NOERROR")

    tryCatch({
        write_file("", begin_filepath, append = FALSE)

        info("=> Combining file ", filename, " \n\n")

        script_count <- nrow(data)

        sequence_number <-
            c(1: (ceiling(script_count / settings$combine_count))) %>%
            rep(each = settings$combine_count) %>%
            `[`(1:script_count)

        output_table <-
            data %>%
            add_column(sequence_number = sequence_number, .before = 1) %>%
            group_by(sequence_number) %>%
            do(combine_group(.data, settings, output_dirpath)) %>%
            ungroup()

        info("\n=> Combined file ", filename, " \n\n")

        write_file("", noerror_filepath)
    },

    error = function(e) {
        write_file("", error_filepath)
        info("=> Error combining ", filename, "\n")
        stop(e)
    },

    finally = {
        write_file("", finish_filepath)
    })

    output_table
}


combine_reduced_data <- function(settings) {

    reduced_analysis_filename_glob <- str_c("*",
                                            data_table_extension(settings$binary,
                                                                 settings$compression_level))

    valid_scripts_filepath <-
        read_csv(settings$valid_scripts_filepath, col_names = FALSE)[[1]]

    scripts <-
        path(settings$input_dirpath, settings$analysis, valid_scripts_filepath) %>%
        dir_ls(type = "file", recursive = FALSE, glob = reduced_analysis_filename_glob) %>%
        { tibble(filepath = path_ext_remove(path_ext_remove(.))) } %>%
        mutate(filename = path_file(filepath),
               script_name = path_file(path_dir(filepath)),
               script_type = path_file(path_dir(path_dir(filepath))),
               package = path_file(path_dir(path_dir(path_dir(filepath))))) %>%
        group_by(filename) %>%
        do(combine_groups(settings, .data)) %>%
        ungroup()
}


parse_program_arguments <- function() {

    usage <- "%prog reduced-output-dirpath combined-output-dirpath analysis combine-count"
    description <- paste(
        "reduced-output-dirpath    directory containing reduced data files (scanned recursively)",
        "combined-output-dirpath   directory to which combined data will be exported",
        "valid-scripts-filepath    file containing valid scripts",
        "analysis                  name of analysis to run",
        "combine-count             number of files to be combined in one step",
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
                    metavar = "compression_level"),

        make_option(c("--combined-filename-prefix"),
                    action = "store",
                    type = "character",
                    default = "part",
                    help = "combined filename prefix",
                    metavar = "combined_filename_prefix")
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
         valid_scripts_filepath = arguments$args[3],
         analysis = arguments$args[4],
         combine_count = as.integer(arguments$args[5]),
         script_type = script_type,
         binary = arguments$options$binary,
         compression_level = arguments$options$compression_level,
         combined_filename_prefix = arguments$options$combined_filename_prefix)
}


main <- function() {
    settings <- parse_program_arguments()
    dir_create(settings$output_dirpath)
    print(settings)
    combine_reduced_data(settings)
}


main()
