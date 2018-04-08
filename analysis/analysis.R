library(methods)
library(tools)
library(gdata)
library(optparse)
library(tidyverse)
library(stringi)
library(ggthemes)
library(scales)
library(crayon)
library(magrittr)
library(lubridate)
library(broom)
library(stringr)

info <- function(...) cat(green(bold(paste0(...))))

file_size <- function(filepath, digits = 2)
  humanReadable(file.info(filepath)$size)

time_difference <- function(time_begin, time_end, units="mins") {
  seconds <- interval(time_begin, time_end) %>% int_length()
  milliseconds <- round(1000 * (seconds %% 1), 0)
  seconds <- seconds %/% 1
  hours <- seconds %/% 3600
  remaining <- seconds %% 3600
  minutes <- remaining %/% 60
  seconds <- remaining %% 60
  paste0(if(hours != 0) paste0(hours, " hours, ") else "",
         if(minutes != 0) paste0(minutes, " minutes, ") else "",
         seconds, " seconds and ",
         milliseconds, " milliseconds")
}

replace_extension <- function(filename, extension)
  filename %>%
    file_path_sans_ext() %>%
    paste0(".", extension)

find_files <- function(input_dirpath, extension)
  list_files_with_exts(input_dirpath, extension) %>%
    keep(
      . %>%
      replace_extension("OK") %>%
      file.exists())

create_table <- function(input_dir) {
  Reduce(bind_rows, lapply(find_files(input_dir, "sqlite"), analyze_database))
}

export_as_images <- function(visualizations, graph_dir, extension = "pdf") {
  visualizations %>%
    iwalk(
      function(graph, graphname) {
        filename <- file.path(graph_dir, paste0(graphname, ".", extension))
        info("  • Exporting ", filename, "\n")
        ggsave(plot = graph, filename = filename)
      })
}

export_as_tables <- function(analyses, table_dir, extension = "csv") {
  analyses %>%
      iwalk(
        function(table, tablename) {
          filename <- file.path(table_dir, paste0(tablename, ".", extension))
          info("  • Exporting ", filename, "\n")
          write_csv(table, filename)
        })
}

import_as_tables <- function(table_dir, extension = "csv") {
  table_files <- list_files_with_exts(table_dir, extension)
  table_names <- map(table_files, compose(file_path_sans_ext, basename))
  analyses <-
    table_files %>%
    map(
      function(table_file) {
          info("  • Importing ", table_file, "\n")
          read_csv(table_file)
      }) %>%
    setNames(table_names)

  analyses
}

combine_analyses <- function(acc, element, combiner = bind_rows) {
  for(name in names(acc)) {
    if(nrow(acc[[name]]) == 0)
      acc[[name]] = element[[name]]
    else if(nrow(element[[name]]) != 0)
      acc[[name]] = combiner(acc[[name]], element[[name]])
  }
  acc
}

parse_program_arguments <- function() {
  usage <- "%prog input-dir table-dir graphs-dir"
  description <- paste(
    "",
    "",
    "part           part of the pipeline to run - analyze, visualize or all",
    "input-dir      directory containing sqlite databases (scanned recursively)",
    "table-dir      directory to which tables will be exported",
    "graph-dir      directory to which graphs will be exported",
    "partial-dir    directory to which partial analyses will be exported",
    "",
    "",
    sep = "\n")

  option_parser <- OptionParser(usage = usage,
                                description = description,
                                add_help_option = TRUE,
                                option_list = list())
  parse_args(option_parser, positional_arguments = 5)
}

drive_analysis <- function(analysis_name,
                           analyze_database,
                           combine_analyses,
                           summarize_analyses,
                           export_analyses,
                           import_analyses,
                           visualize_analyses,
                           export_visualizations) {
  info(analysis_name, "\n")

  start_time <- now()

  arguments <- parse_program_arguments()
  part <- arguments$args[1]
  input_dir <- arguments$args[2]
  table_dir <- arguments$args[3]
  graph_dir <- arguments$args[4]
  partial_dir <- arguments$args[5]

  if(part %in% c("all", "analyze")) {
    analyses <-
      input_dir %T>%
      info("• Sanning for sqlite files in ", ., "\n") %>%
      find_files("sqlite") %T>%
      {info("  • Found ", length(.), " files", "\n", "• Analyzing databases", "\n")} %>%
      lapply(
        function(database_filepath) {
          info("  • Analyzing ", database_filepath,
               " (", file_size(database_filepath), ")", "\n")
          result <- analyze_database(database_filepath)
          if(!is.null(result)) {
            info("  • Exporting analysis", "\n")
            analysis_dir <- file.path(partial_dir, file_path_sans_ext(basename(database_filepath)))
            dir.create(analysis_dir)
            export_as_tables(result, analysis_dir)
          }
          result
        }) %>%
      compact()
  }

  if(part %in% c("all", "summarize")) {
    info("• Importing analyses", "\n")
    analyses <-
      list.dirs(partial_dir, recursive = FALSE) %>%
      lapply(import_as_tables) %T>%
      (function (ignore) info("• Combining analyses", "\n")) %>%
      Reduce(combine_analyses, .) %T>%
      (function (ignore) info("• Summarizing analyses", "\n")) %>%
      summarize_analyses()
    info("• Exporting summary", "\n")
    export_analyses(analyses, table_dir)
  }

  if(part %in% c("all", "visualize")) {
    info("• Importing summary", "\n")
    analyses <- import_analyses(table_dir)

    info("• Visualizing analyses", "\n")
    visualizations <- visualize_analyses(analyses)

    info("• Exporting visualizations", "\n")
    export_visualizations(visualizations, graph_dir)
  }

  info("• Finished in ", time_difference(start_time, now()), "\n")

  warnings()
}
