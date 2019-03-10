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


merge_combined_data <- function(settings, combined_data_table) {

    read_combined_data_part_file <- function(combined_data_part_filepath, pb) {
        data_table <-
            read_data_table(path_ext_remove(path_ext_remove(combined_data_part_filepath)),
                            binary = settings$binary,
                            compression_level = settings$compression_level)

        pb$tick(tokens = list(part_filename = path_file(combined_data_part_filepath)))

        data_table
    }

    combined_data_table %>%
        pmap_dfr(function(analysis, data_table_name, combined_data_dirpath, finish) {

            if (!finish) {
                info("Combined data table '",
                     data_table_name,
                     "'was not created successfully.\n")
            }
            else {
                info("Reading '", combined_data_dirpath, "'\n")

                combined_data_part_filepaths <-
                    combined_data_dirpath %>%
                    dir_ls(recursive = FALSE,
                           type = "file",
                           regexp = str_c(".*\\d{6}\\.",
                                          data_table_extension(settings$binary,
                                                               settings$compression_level),
                                          sep = ""))

                merged_data_filepath <- path(settings$output_dirpath, data_table_name)

                pb <- progress_bar$new(format = ":part_filename [:bar] :percent :eta",
                                       total = length(combined_data_part_filepaths),
                                       clear = FALSE,
                                       width = 100)

                combined_data_part_filepaths %>%
                    map_dfr(read_combined_data_part_file, pb) %>%
                    write_data_table(merged_data_filepath,
                                     truncate = TRUE,
                                     binary = settings$binary,
                                     compression_level = settings$compression_level)

                info("Writing '", merged_data_filepath, "'\n")

                gc()

                tibble(combined_data_dirpath = combined_data_dirpath,
                       merged_data_filepath = merged_data_filepath)
            }
        })
}


scan_input_dirpath <- function(settings) {

    info("=> Scanning for combined data files in ", settings$input_dirpath, "\n")

    combined_data <-
        settings$input_dirpath %>%
        path(settings$analysis) %>%
        dir_ls(type = "directory") %>%
        map_dfr(function(combined_data_dirpath) {
            finish_filepath <- path(combined_data_dirpath, "FINISH")

            tibble(analysis = settings$analysis,
                   data_table_name = path_file(combined_data_dirpath),
                   combined_data_dirpath = combined_data_dirpath,
                   finish = file_exists(finish_filepath))
        })

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
    print(combined_data)
    print(merge_combined_data(settings, combined_data))
}


main()
