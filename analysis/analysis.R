suppressPackageStartupMessages(library(optparse))
suppressPackageStartupMessages(library(methods))
suppressPackageStartupMessages(library(tools))
suppressWarnings(suppressPackageStartupMessages(library(tidyverse)))
suppressWarnings(suppressPackageStartupMessages(library(ggthemes)))
suppressWarnings(suppressPackageStartupMessages(library(scales)))
suppressWarnings(suppressPackageStartupMessages(library(crayon)))
suppressWarnings(suppressPackageStartupMessages(library(magrittr)))

info <- function(...) cat(green(bold(paste0(...))))

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

export_as_images <- function(visualizations, graph_dir, format = ".png") {
  visualizations %>%
    iwalk(
      function(graph, graphname)
        ggsave(plot = graph,
               filename = file.path(graph_dir,
                                    paste0(graphname, format))))

}

export_as_tables <- function(analyses, table_dir, format = ".csv") {
  analyses %>%
      iwalk(
        function(table, tablename)
          table %>%
          write_csv(file.path(table_dir, paste0(tablename, format))))
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
                           summarize_analyses,
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
    file.path("data") %T>%
    info("• Recursively scanning sqlite files in ", ., "\n") %>%
    find_files("sqlite") %T>%
    {info("  • Found ", length(.), " files", "\n", "• Analyzing databases", "\n")} %>%
    lapply(analyze_database) %T>%
    (function (ignore) info("• Combining analyses", "\n")) %>%
    Reduce(combine_analyses, .) %T>%
    (function (ignore) info("• Summarizing analyses", "\n")) %>%
    summarize_analyses()

  info("• Exporting analyses", "\n")
  export_analyses(analyses, table_dir)

  info("• Visualizing analyses", "\n")
  visualizations <- visualize_analyses(analyses)

  info("• Exporting visualizations", "\n")
  export_visualizations(visualizations, graph_dir)

  info("• Finished in ", (Sys.time() - start_time) , " minutes", "\n")

}
