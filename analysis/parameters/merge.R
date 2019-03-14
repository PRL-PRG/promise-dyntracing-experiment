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

read_combined_data_part_file <- function(combined_data_part_filepath, settings, pb) {

    pb$tick(tokens = list(part_filename = path_file(combined_data_part_filepath)))

    data_table <-
        read_data_table(path_ext_remove(path_ext_remove(combined_data_part_filepath)),
                        binary = settings$binary,
                        compression_level = settings$compression_level)

    data_table
}

merge_combined_file <- function(analysis, data_table_name, combined_data_dirpath,
                                valid, settings) {

    info("=> Merging combined data files for '", data_table_name, "'\n")

    if (valid) {

        merged_data_filepath <- path(settings$output_dirpath, data_table_name)

        info("Reading '", combined_data_dirpath, "'\n")

        combined_data_part_filepaths <-
            combined_data_dirpath %>%
            dir_ls(recursive = FALSE,
                   type = "file",
                   regexp = str_c(".*\\d{6}\\.",
                                  data_table_extension(settings$binary,
                                                       settings$compression_level),
                                  sep = ""))

        pb <- progress_bar$new(format = ":part_filename [:bar] :percent :eta",
                               total = length(combined_data_part_filepaths),
                               clear = FALSE,
                               width = 100)

        combined_data_part_filepaths %>%
            map_dfr(read_combined_data_part_file, settings, pb) %>%
            write_data_table(merged_data_filepath,
                             truncate = TRUE,
                             binary = settings$binary,
                             compression_level = settings$compression_level)

        info("Writing '", merged_data_filepath, "'\n")

        tibble(combined_data_dirpath = combined_data_dirpath,
               merged_data_filepath = merged_data_filepath)
    }
    else {

        info("Invalid '", data_table_name, "'\n")

        tibble(combined_data_dirpath = "", merged_data_filepath = "")
    }
}


merge_combined_data <- function(settings, combined_data_table) {

    combined_data_table %>%
        pmap_dfr(merge_combined_file, settings)

}


scan_input_dirpath <- function(settings) {

    info("=> Scanning for combined data files in ", settings$input_dirpath, "\n")

    combined_data <-
        settings$input_dirpath %>%
        path(settings$analysis) %>%
        dir_ls(type = "directory", recursive = FALSE) %>%
        map_dfr(function(combined_data_dirpath) {
            begin_filepath <- path(combined_data_dirpath, "BEGIN")
            finish_filepath <- path(combined_data_dirpath, "FINISH")
            error_filepath <- path(combined_data_dirpath, "ERROR")
            noerror_filepath <- path(combined_data_dirpath, "NOERROR")

            tibble(analysis = settings$analysis,
                   data_table_name = path_file(combined_data_dirpath),
                   combined_data_dirpath = combined_data_dirpath,
                   begin = file_exists(begin_filepath),
                   finish = file_exists(finish_filepath),
                   error = file_exists(error_filepath),
                   noerror = file_exists(noerror_filepath)) %>%
                mutate(valid = begin & finish & noerror & (!error)) %>%
                select(analysis, data_table_name, combined_data_dirpath, valid)
        })

    info("=> Found ", nrow(combined_data), " combined files in ", settings$input_dirpath, "\n\n")

    combined_data
}


parse_program_arguments <- function() {

    usage <- "%prog combined-input-dirpath merged-output-dirpath analysis"
    description <- paste(
        "combined-input-dirpath  directory containing combined data files",
        "merged-output-dirpath   directory to which merged data files will be exported",
        "analysis                name of analysis to run",
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

    list(input_dirpath = arguments$args[1],
         output_dirpath = arguments$args[2],
         analysis = arguments$args[3],
         binary = arguments$options$binary,
         compression_level = arguments$options$compression_level)

}


main <- function() {
    settings <- parse_program_arguments()

    dir_create(settings$output_dirpath)

    print(settings)

    combined_data <- scan_input_dirpath(settings)

    merge_combined_data(settings, combined_data)

    invisible(NULL)
}


main()
