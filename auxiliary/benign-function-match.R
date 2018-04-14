#!/usr/bin/env Rscript

library(optparse)
library(tidyverse)

parse_program_arguments <- function() {
  usage <- "%prog input-dir function-table"
  description <- paste(
    "",
    "",
    "side-effects-table-dir   directory containing summary of side effects",
    "function-database        sqlite database containing all functions",
    "",
    "",
    sep = "\n")

  option_parser <- OptionParser(usage = usage,
                                description = description,
                                add_help_option = TRUE,
                                option_list = list())
  parse_args(option_parser, positional_arguments = 2)
}


main <- function() {

  arguments <- parse_program_arguments()
  input_dir <- arguments$args[1]
  function_database <- arguments$args[2]

  functions <-
    src_sqlite(function_database) %>%
    tbl("functions") %>%
    select(id, definition) %>%
    collect()

  read_csv(file.path(input_dir, "benign_functions.csv")) %>%
    left_join(functions) %>%
    write_csv(file.path(input_dir, "benign_function_definitions.csv"))
}

main()
