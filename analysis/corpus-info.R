#!/usr/bin/env Rscript

source("analysis/utils.R")
source("analysis/analysis.R")

library(dplyr)

analyze_database <- function(database_file_path) {
  components <- stringr::str_split(
    basename(tools::file_path_sans_ext(database_file_path)), "-", 2)[[1]]
  
  db <- src_sqlite(database_file_path)
  promises <- tbl(db, sql('SELECT COUNT(*) AS n FROM promises')) %>% pull(n)
  promise_events <- tbl(db, sql('SELECT COUNT(*) AS n FROM promise_lifecycle')) %>% pull(n)
  promise_evaluations <- tbl(db, sql('SELECT COUNT(*) AS n FROM promise_lifecycle WHERE event_type = 1')) %>% pull(n)
  promise_lookups <- tbl(db, sql('SELECT COUNT(*) AS n FROM promise_lifecycle WHERE event_type = 5')) %>% pull(n)
  calls <- tbl(db, sql('SELECT COUNT(*) AS n FROM calls')) %>% pull(n)
  functions <- tbl(db, sql('SELECT id FROM functions')) %>% collect
  
  list(general=data.frame(package = components[1],
                          vignette = components[2],
                          calls = calls,
                          promises = promises,
                          promise_events = promise_events,
                          promise_evaluations = promise_evaluations,
                          promise_lookups = promise_lookups),
       functions=data.frame(functions = functions))
}

summarize_analyses <- function(analyses) {
  packages <- analyses$general %>% pull(package) %>% unique %>% length
  vignettes <- analyses$general %>% pull(vignette) %>% unique %>% length
  functions <- analyses$functions %>% pull(id) %>% unique %>% length
  calls <- analyses$general %>% pull(calls) %>% sum
  promises <- analyses$general %>% pull(promises) %>% sum
  promise_events <- analyses$general %>% pull(promise_events) %>% sum
  promise_evaluations <- analyses$general %>% pull(promise_evaluations) %>% sum
  promise_lookups <- analyses$general %>% pull(promise_lookups) %>% sum
  list(general=data.frame(packages = packages,
                          vignettes = vignettes,
                          functions = functions,
                          calls = calls,
                          promises = promises,
                          promise_events = promise_events,
                          promise_evaluations = promise_evaluations,
                          promise_lookups = promise_lookups))
}

visualize_analyses <- function(analyses) {
  list()
}

latex_analyses <- function(analyses) {
  list()
}

main <-
  function() {
    analyzer <-
      create_analyzer("Corpus Information Analysis",
                      analyze_database,
                      combine_analyses,
                      summarize_analyses,
                      visualize_analyses,
                      latex_analyses)
    drive_analysis(analyzer)
  }

main()
warnings()
