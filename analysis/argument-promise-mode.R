#!/usr/bin/env Rscript

suppressPackageStartupMessages(library(methods))
suppressPackageStartupMessages(library(tools))
suppressPackageStartupMessages(library(optparse))
suppressWarnings(suppressPackageStartupMessages(library(tidyverse)))

databases <- function(input_dirpath)
  Filter(function(filename) { file_ext(filename) == "sqlite" },
         list.files(path=input_dirpath,
                    full.names=TRUE,
                    recursive=TRUE,
                    include.dirs=FALSE,
                    no..=TRUE))

create_table <- function(input_dir) {
  Reduce(bind_rows, lapply(databases(input_dir), analyze))
}

analyze <- function(database_filepath) {
  db <- src_sqlite(database_filepath)
  promises_tbl <- tbl(db, "promises")
  promise_argument_types_tbl <- tbl(db, "promise_argument_types")

  total_promises <-
    promises_tbl %>%
    summarize(`COUNT` = n()) %>%
    collect() %>%
    mutate(`ARGUMENT TYPE` = "ALL")

  promise_argument_types_tbl %>%
    collect() %>%
    distinct(promise_id, .keep_all=TRUE) %>%
    select(`ARGUMENT TYPE`=default_argument) %>%
    group_by(`ARGUMENT TYPE`) %>%
    summarize(`COUNT`=n()) %>%
    mutate(`ARGUMENT TYPE`=replace(`ARGUMENT TYPE`, `ARGUMENT TYPE`==0, "NON-DEFAULT ARGUMENT")) %>%
    mutate(`ARGUMENT TYPE`=replace(`ARGUMENT TYPE`, `ARGUMENT TYPE`==1, "DEFAULT ARGUMENT")) %>%    
    bind_rows(total_promises) %>%
    mutate(`RUNNABLE`=basename(file_path_sans_ext(database_filepath)))
}

export_table <- function(datatable, output_file) {
  datatable %>%
    spread(`ARGUMENT TYPE`, `COUNT`) %>%
    mutate(`UNACCOUNTED` = `ALL` - (`NON-DEFAULT ARGUMENT` + `DEFAULT ARGUMENT`)) %>%
    replace(., is.na(.), 0) %>%
    write_tsv(output_file)
}

parse_program_arguments <- function() {
  usage <- "%prog input-dir table-dir graphs-dir"
  description <- paste(
    "",
    "",
    "input-dir    directory containing sqlite databases (scanned recursively)",
    "table-dir    directory to which tables will be exported",
    "graph-dir    directory to which graphs will be exported",
    "",
    "",
    sep = "\n")

  option_parser <- OptionParser(usage = usage,
                                description = description, 
                                add_help_option = TRUE,
                                option_list = list())
  parse_args(option_parser, positional_arguments = 3)
}

main <- function() {
  arguments <- parse_program_arguments()
  input_dir = arguments$args[1]
  table_dir = arguments$args[2]
  graph_dir = arguments$args[3]
  datatable <- create_table(input_dir)
  export_table(datatable, file.path(table_dir, "argument-promise-mode.tsv"))
  ## analyze_table(datatable,
  ##               file.path(table_dir, "environment-usage-lower-quartile.tsv"),
  ##               file.path(table_dir, "environment-usage-upper-quartile.tsv"))
  ## create_graph(datatable, file.path(graph_dir, "fun-"))
}

main()
