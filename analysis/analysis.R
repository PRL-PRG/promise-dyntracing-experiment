suppressPackageStartupMessages(library(optparse))
suppressPackageStartupMessages(library(methods))
suppressPackageStartupMessages(library(tools))
suppressWarnings(suppressPackageStartupMessages(library(tidyverse)))
suppressWarnings(suppressPackageStartupMessages(library(ggthemes)))

find_files <- function(input_dirpath, extension)
  Filter(function(filename) { file_ext(filename) == extension },
         list.files(path=input_dirpath,
                    full.names=TRUE,
                    recursive=TRUE,
                    include.dirs=FALSE,
                    no..=TRUE))

create_table <- function(input_dir) {
  Reduce(bind_rows, lapply(find_files(input_dir, "sqlite"), analyze_database))
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

drive_analysis <- function(analyze_database,
                           combine_analyses,
                           export_analyses,
                           visualize_analyses,
                           export_visualizations) {
  start_time <- Sys.time()

  arguments <- parse_program_arguments()
  input_dir = arguments$args[1]
  table_dir = arguments$args[2]
  graph_dir = arguments$args[3]

  analyses <-
    input_dir %>%
    find_files("sqlite") %>%
    lapply(analyze_database) %>%
    Reduce(combine_analyses, .)

  export_analyses(analyses, table_dir)

  visualizations <- visualize_analyses(analyses)

  export_visualizations(visualizations, graph_dir)

  Sys.time() - start_time

}
