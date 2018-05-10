#!/usr/bin/env Rscript

source("analysis/utils.R")
source("analysis/analysis.R")

library(dplyr)

analyze_database <- function(database_file_path) {
  components <- split_path_to_components(database_file_path)
  
  db <- src_sqlite(database_file_path)
  promises <- tbl(db, sql('SELECT COUNT(*) AS n FROM promises')) %>% pull(n)
  promise_events <- tbl(db, sql('SELECT COUNT(*) AS n FROM promise_lifecycle')) %>% pull(n)
  promise_evaluations <- tbl(db, sql('SELECT COUNT(*) AS n FROM promise_lifecycle WHERE event_type = 15')) %>% pull(n)
  promise_lookups <- tbl(db, sql('SELECT COUNT(*) AS n FROM promise_lifecycle WHERE event_type = 1')) %>% pull(n)
  calls <- tbl(db, sql('SELECT COUNT(*) AS n FROM calls')) %>% pull(n)
  functions <- tbl(db, sql('SELECT id FROM functions')) %>% collect
  
  list(general=data.frame(package = components$package,
                          runnable = components$title,
                          calls = calls,
                          promises = promises,
                          promise_events = promise_events,
                          promise_evaluations = promise_evaluations,
                          promise_lookups = promise_lookups),
       functions=data.frame(functions = functions))
}

summarize_analyses <- function(analyses) {
  packages <- analyses$general %>% pull(package) %>% unique %>% length
  runnables <- analyses$general %>% pull(runnable) %>% unique %>% length
  functions <- analyses$functions %>% pull(id) %>% unique %>% length
  calls <- analyses$general %>% pull(calls) %>% sum
  promises <- analyses$general %>% pull(promises) %>% sum
  promise_events <- analyses$general %>% pull(promise_events) %>% sum
  promise_evaluations <- analyses$general %>% pull(promise_evaluations) %>% sum
  promise_lookups <- analyses$general %>% pull(promise_lookups) %>% sum
  list(general=data.frame(packages = packages,
                          runnables = runnables,
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
