#!/usr/bin/env Rscript

source("analysis/utils.R")
source("analysis/analysis.R")

library(dplyr)

# expects a certain set of functions

# MAP
# for every DB file
analyze_database <- function(database_file_path) {
  components <- stringr::str_split(
    basename(tools::file_path_sans_ext(database_file_path)), "-", 2)[[1]]
  
  db <- src_sqlite(database_file_path)
  promises <- db %>% tbl("promises")
  promise_evaluations <- db %>% tbl("promise_evaluations") %>% group_by(event_type) %>% count
  calls <- db %>% tbl("calls") 
  functions <- db %>% tbl("functions") 
  
  promise_evaluations <- 
    left_join(data.frame(event_type=c(0,15)), promise_evaluations, by='event_type', copy=TRUE) %>% 
    mutate(n=ifelse(is.na(n), 0, n)) %>% collect
  
  list(general=data.frame(db=database_file_path, 
                          package = components[1],
                          vignette = components[2],
                          functions = functions %>% count %>% pull(n),
                          calls = calls %>% count %>% pull(n),
                          promises = promises %>% count %>% pull(n),
                          promise_forces = promise_evaluations %>% filter(event_type == 15) %>% pull(n),
                          promise_lookups = promise_evaluations %>% filter(event_type == 0) %>% pull(n)))
}

# REDUCE
# runs analyses on all the data from all the vignettes
# analyses is the list form combine analyses
# returns the summarized data that undergoes visualization
summarize_analyses <- function(analyses) {
  list(general=data.frame(packages = analyses$general %>% pull(package) %>% unique %>% length,
                  vignette = analyses$general %>% pull(vignette) %>% unique %>% length,
                  functions = analyses$general %>% pull(functions) %>% sum,
                  calls = analyses$general %>% pull(calls) %>% sum,
                  promises = analyses$general %>% pull(promises) %>% sum,
                  promise_forces = analyses$general %>% pull(promise_forces) %>% sum,
                  promise_lookups = analyses$general %>% pull(promise_lookups) %>% sum))
}

#return a list of ggplot2 objects
visualize_analyses <- function(analyses) {
  list()
}


main <- function() {  
  drive_analysis("general-info",
                 analyze_database,
                 combine_analyses,
                 summarize_analyses, 
                 export_as_tables,  # exports CSV files automatically
                 import_as_tables,  # (re-)imports data from CSV files
                 visualize_analyses,
                 export_as_images)
}

main()




