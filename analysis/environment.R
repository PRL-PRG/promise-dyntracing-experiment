#!/usr/bin/env Rscript

suppressPackageStartupMessages(library(methods))
suppressPackageStartupMessages(library(tools))
suppressPackageStartupMessages(library(optparse))
suppressWarnings(suppressPackageStartupMessages(library(tidyverse)))

ENVIRONMENT_METHODS <- c("environment",
                         "environment<-",
                         "parent.env",
                         "parent.env<-",
                         "globalenv",
                         "baseenv",
                         "emptyenv",
                         "new.env")

analyze <- function(database_filepath) {

    db <- src_sqlite(database_filepath)
    environments_tbl <- tbl(db, "environments")
    calls_tbl <- tbl(db, "calls") 

    total_calls <-
      calls_tbl %>%
      summarize(`COUNT` = n()) %>%
      collect() %>%
      mutate(`FUNCTION NAME`="TOTAL CALLS")

    total_new_environments <-
      environments_tbl %>%
      summarize(`COUNT` = n()) %>%
      collect() %>%
      mutate(`FUNCTION NAME`="NewEnvironment")

    environment_functions <-
      calls_tbl %>%
      select(`FUNCTION NAME`=function_name) %>%
      filter(`FUNCTION NAME` %in% ENVIRONMENT_METHODS) %>%
      group_by(`FUNCTION NAME`) %>%
      summarize(`COUNT` = n()) %>%
      collect() %>%
      bind_rows(total_calls, total_new_environments) %>%
      mutate(`RUNNABLE`=basename(file_path_sans_ext(database_filepath)))
    
    environment_functions
}

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

analyze_table <- function(datatable,
                          lower_quartile_datafile,
                          upper_quartile_datafile) {

  ## Filter scripts which fall in the lower quartile for each environment
  ## function call
  datatable %>%
    group_by(`FUNCTION NAME`) %>%
    do({
        filter(.data=., `COUNT` <= unname(quantile(`COUNT`, 0.25)))
    }) %>%
    spread(`FUNCTION NAME`, `COUNT`) %>%
    write_tsv(lower_quartile_datafile)

  ## Filter scripts which fall in the upper quartile for each environment
  ## function call
  datatable %>%
    group_by(`FUNCTION NAME`) %>%
    do({
        filter(.data=., `COUNT` >= unname(quantile(`COUNT`, 0.75)))
    }) %>%
    spread(`FUNCTION NAME`, `COUNT`) %>%
    write_tsv(upper_quartile_datafile)

  datatable
}

export_table <- function(datatable, output_file) {
  datatable %>%
    spread(`FUNCTION NAME`, `COUNT`) %>%
    replace(., is.na(.), 0) %>%
    write_tsv(output_file)
}

create_graph <- function(datatable, output_file) {
  #https://stackoverflow.com/questions/29034863/apply-a-ggplot-function-per-group-with-dplyr-and-set-title-per-group
  datatable %>%
    group_by(`FUNCTION NAME`) %>%
    do({
      plot <-
        ggplot(data=., aes(`FUNCTION NAME`, `COUNT`)) +
        geom_violin() +
        geom_boxplot(width = 0.2)
      filename <- paste0(output_file,.$`FUNCTION NAME`[1],".png")
      ggsave(plot=plot, file=filename)
      data.frame(filename=filename)
      })
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
  export_table(datatable, file.path(table_dir, "environment-usage.tsv"))
  analyze_table(datatable,
                file.path(table_dir, "environment-usage-lower-quartile.tsv"),
                file.path(table_dir, "environment-usage-upper-quartile.tsv"))
  create_graph(datatable, file.path(graph_dir, "fun-"))
}

main()
