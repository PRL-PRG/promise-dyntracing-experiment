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

export_as_images <- function(visualizations, graph_dir, logger, extension = "pdf") {

  dir.create(graph_dir, showWarnings = FALSE, recursive = TRUE)

  visualizations %>%
    iwalk(
      function(graph, graphname) {
        filename <- file.path(graph_dir, paste0(graphname, ".", extension))
        info("  • Exporting ", filename, "\n")
        ggsave(plot = graph, filename = filename)
      })
}

export_as_tables <- function(analyses, table_dir, logger, extension = "csv") {

  dir.create(table_dir, showWarnings = FALSE, recursive = TRUE)

  analyses %>%
      iwalk(
        function(table, tablename) {
          filename <- file.path(table_dir, paste0(tablename, ".", extension))
          info("  • Exporting ", filename, "\n")
          write_csv(table, filename)
        })
}


import_as_tables <- function(table_dir, logger, extension = "csv") {
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


create_latex_def <-
  function(value, name) {
    paste("\\def",
          paste0("\\", name),
          paste0("{", value, "}"),
          sep = " ")
  }


create_latex_defs <-
  function(def_pairs) {
    def_pairs %>%
      map2(names(.), create_latex_def) %>%
      paste0(collapse="\n")
  }


export_as_latex_defs <-
  function(def_pairs, analysis_name, latex_dir, logger) {
    dir.create(latex_dir, showWarnings = FALSE, recursive = TRUE)

    latex_filename <- file.path(latex_dir, "variables.sty")

    latex_file <- file(latex_filename, "wt")

    comment_line <- paste0(rep("%", 80), collapse = "")

    writeLines(comment_line, latex_file)
    writeLines("%%", latex_file)
    writeLines(paste0("%% ", analysis_name), latex_file)
    writeLines("%%", latex_file)
    writeLines(comment_line, latex_file)
    writeLines("\n", latex_file)
    writeLines(create_latex_defs(def_pairs), latex_file)

    close(latex_file)
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

  usage <- "%prog input-dir summary-dir visualization-dir latex-filename cache-dir [OPTION]..."
  description <- paste(
    "",
    "input-dir         directory containing sqlite databases (scanned recursively)",
    "summary-dir       directory to which summaries will be exported",
    "visualization-dir directory to which visualizations will be exported",
    "latex-filename    file to which latex will be exported",
    "cache-dir         directory to which intermediate state will be exported",
    sep = "\n")

  option_list <- list(make_option(c("-c", "--use-cache"),
                                  action="store_true",
                                  default=FALSE,
                                  help="use cache for pre-computed results [default %default]"),
                      make_option(c("-s", "--stage"),
                                  type="character",
                                  default="all",
                                  help="stage of the pipeline to run - analyze, summarize, visualize, latex or all [default %default]",
                                  metavar="character"))

  option_parser <- OptionParser(usage = usage,
                                description = description,
                                add_help_option = TRUE,
                                option_list = option_list)

  arguments <- parse_args2(option_parser) #, positional_arguments = 5)

  list(stage=arguments$options$stage,
       use_cache=arguments$options$use_cache,
       input_dir=arguments$args[1],
       summary_dir=arguments$args[2],
       visualization_dir=arguments$args[3],
       latex_filename=arguments$args[4],
       cache_dir=arguments$args[5])
}

underscore_to_camel_case <-
  function(name) {
    name %>%
      str_replace_all("_", " ") %>%
      str_to_title() %>%
      str_replace_all(" ", "")
  }

to_list <-
  function(df, row) {
    setNames(as.vector(df[row,]),
             underscore_to_camel_case(colnames(df)))
  }


fix_string <-
  function(prefix="", suffix="") {
    function(str) {
      paste0(paste0(prefix, collapse=""),
             str,
             paste0(suffix, collapse=""))
    }
  }


scan_stage <-
  function(analyzer, logger, settings) {

    info("• Sanning for sqlite files in ", settings$input_dir, "\n")

    found_files <- find_files(settings$input_dir, "sqlite")
    filenames <- basename(file_path_sans_ext(found_files))
    cached_files <- file.path(settings$cache_dir, filenames)
    exists <- file.exists(cached_files)
    compute <- !settings$use_cache | (settings$use_cache & !exists)

    tibble(filename=filenames,
           source_path=found_files,
           cache_path=cached_files,
           exists=exists,
           compute=compute)
  }

analyze_stage <-
  function(analyzer, logger, settings, scan) {

    analyze <-
      function(filename, source_path, cache_path, exists, compute) {
        if(compute) {
          info("• Analyzing ", source_path, " (", file_size(source_path), ")", "\n")
          result <- analyzer$analyze_database(source_path)
          if(!is.null(result)) {
            info("• Exporting analysis", "\n")
              dir.create(cache_path, showWarnings = FALSE, recursive = TRUE)
              analyzer$export_analysis(result, cache_path, logger)
          }
        } else {
          info("• Skipping ", source_path, " (", file_size(source_path), ") ",
               "partial data already exists at ", cache_path,
               " and overwrite is not set", "\n")
          result <- analyzer$import_analysis(cache_path, logger)
        }
        result
      }

    info("\n", "• Analyzing databases", "\n")

    analyses <-
      scan %>%
      as.list() %>%
      pmap(analyze) %>%
      compact()

    info("\n", "• Analyzed databases", "\n")

    analyses
  }

combine_stage <-
  function(analyzer, logger, settings, analyses) {
    info("• Combining analyses", "\n")
    combined_analyses <- Reduce(analyzer$combine_analyses, analyses)
    info("\n", "• Combined databases", "\n")

    combined_analyses
  }


summarize_stage <-
  function(analyzer, logger, settings, combined_analyses) {
    info("• Summarizing analyses", "\n")

    summarized_analyses <- analyzer$summarize_analyses(combined_analyses)

    info("• Exporting summary", "\n")
    analyzer$export_summary(summarized_analyses,
                            settings$summary_dir,
                            logger)

    summarized_analyses
  }



visualize_stage <-
  function(analyzer, logger, settings, summarized_analyses) {
    info("• Visualizing analyses", "\n")
    visualizations <- analyzer$visualize_analyses(summarized_analyses)

    info("• Exporting visualizations", "\n")
    analyzer$export_visualizations(visualizations,
                                   settings$visualization_dir,
                                   logger)
    visualizations
  }


latex_stage <-
  function(analyzer, logger, settings, summarized_analyses) {
    info("• Latexing analyses", "\n")
    latex <- analyzer$latex_analyses(summarized_analyses)

    info("• Exporting latex to ", settings$latex_filename, "\n")
    analyzer$export_latex(latex,
                          analyzer$name,
                          settings$latex_filename,
                          logger)
  }

import_analyses <-
  function(analyzer, logger, settings) {
    list.dirs(settings$cache_dir, recursive = FALSE) %>%
      map2(list(logger), analyzer$import_analysis)
  }

import_summarized_analyses <-
  function(analyzer, logger, settings) {
    analyzer$import_summary(settings$summary_dir, logger)
  }

create_analyzer <- function(analysis_name,
                            analyze_database,
                            combine_analyses,
                            summarize_analyses,
                            visualize_analyses,
                            latex_analyses,
                            import_analysis = import_as_tables,
                            export_analysis = export_as_tables,
                            import_summary = import_as_tables,
                            export_summary = export_as_tables,
                            export_visualizations = export_as_images,
                            export_latex = export_as_latex_defs) {
  analyzer <- list(name=analysis_name,
                   analyze_database=analyze_database,
                   combine_analyses=combine_analyses,
                   summarize_analyses=summarize_analyses,
                   visualize_analyses=visualize_analyses,
                   latex_analyses=latex_analyses,
                   import_analysis=import_analysis,
                   export_analysis=export_analysis,
                   import_summary=import_summary,
                   export_summary=export_summary,
                   export_visualizations=export_visualizations,
                   export_latex=export_latex)
  class(analyzer) <- c("analyzer")
  analyzer
}

create_logger <- function() {
  indentation <- 0
  indent <- function() { indentation <- indentation + 1 }
  exdent <- function() { indentation <- indentation - 1 }
  logger <- list(info = info,
                 indent = indent,
                 exdent = exdent)
  class(logger) <- c("logger")
  logger
}

drive_analysis <-
  function(analyzer,
           logger = create_logger(),
           settings = parse_program_arguments()) {

    start_time <- now()

    info(analyzer$name, "\n")

    analyses <- NULL
    combined_analyses <- NULL
    summarized_analyses <- NULL
    latex <- NULL
    visualizations <- NULL
    stage <- settings$stage

    if(stage %in% c("all", "analyze")) {
      scan <- scan_stage(analyzer, logger, settings)
      analyses <- analyze_stage(analyzer, logger, settings, scan)
    }

    if(stage %in% c("all", "summarize")) {
      if(is.null(analyses)) {
        info("• Importing analyses", "\n")
        analyses <- import_analyses(analyzer, logger, settings)
      }
      combined_analyses <- combine_stage(analyzer, logger, settings, analyses)
      summarized_analyses <- summarize_stage(analyzer, logger, settings, combined_analyses)
    }

    if(stage %in% c("all", "latex")) {
      if(is.null(summarized_analyses)) {
        info("• Importing summary", "\n")
        summarized_analyses <- import_summarized_analyses(analyzer, logger, settings)
      }
      latex_stage(analyzer, logger, settings, summarized_analyses)
    }

    if(stage %in% c("all", "visualize")) {
      if(is.null(summarized_analyses)) {
        info("• Importing summary", "\n")
        summarized_analyses <- import_summarized_analyses(analyzer, logger, settings)
      }
      visualize_stage(analyzer, logger, settings, summarized_analyses)
    }

    end_time <- now()

    info("• Finished in ", time_difference(start_time, end_time), "\n")

  }
