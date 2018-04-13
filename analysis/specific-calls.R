#!/usr/bin/env Rscript

source("analysis/utils.R")
source("analysis/analysis.R")

library(dplyr)

analyze_database <- function(database_file_path) {
  components <- stringr::str_split(
    basename(tools::file_path_sans_ext(database_file_path)), "-", 2)[[1]]
  
  db <- src_sqlite(database_file_path)
  
  list(
    specific_calls=data <- db %>%
      tbl("calls") %>% 
      select(function_name) %>% 
      collect %>% 
      filter(grepl("delayedAssign", function_name)) %>% 
      group_by(function_name) %>% 
      count %>% 
      as.data.frame
  )
}

summarize_analyses <- function(analyses) {
  list(specific_calls = analyses$specific_calls %>% group_by(function_name) %>% summarise(number=sum(n)))
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
      create_analyzer("Specific Calls Analysis",
                      analyze_database,
                      combine_analyses,
                      summarize_analyses,
                      visualize_analyses,
                      latex_analyses)
    drive_analysis(analyzer)
  }

main()
warnings()
