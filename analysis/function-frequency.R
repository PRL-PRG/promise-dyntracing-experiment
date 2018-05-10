#!/usr/bin/env Rscript

source("analysis/utils.R")
source("analysis/analysis.R")

library(dplyr)

analyze_database <- function(database_file_path) {
  db <- src_sqlite(database_file_path)
  
  list(
    specific_calls=data <- db %>%
      tbl("calls") %>% 
      select(function_id, function_name) %>% 
      collect %>% 
      group_by(function_id, function_name) %>% 
      count %>%
      rename(id=function_id, number=n, alias=function_name) %>%
      as.data.frame
  )
}

summarize_analyses <- function(analyses) {
  list(specific_calls = analyses$specific_calls %>% 
      as.data.frame %>%
      group_by(id, alias) %>%
      summarise(number=sum(as.numeric(number))) %>%
      mutate(percent=100*number/sum(number)) %>%
      ungroup %>%
      mutate(aliases=paste0(alias, "[", round(percent), "%]")) %>%
      arrange(desc(number)) %>%
      group_by(id) %>%
      summarize(name=alias[1],
                aliases=paste(aliases, sep=" ", collapse=" "),
                number=sum(number)) %>% 
      ungroup %>%
      mutate(percent=(100*number/sum(number))) %>%
      arrange(desc(number)) %>%
      mutate(number=count_labels(number), percent=percent_labels(percent)) %>%
      select(name, aliases, number, percent) 
  )
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
      create_analyzer("Specific Call Analysis",
                      analyze_database,
                      combine_analyses,
                      summarize_analyses,
                      visualize_analyses,
                      latex_analyses)
    drive_analysis(analyzer)
  }

main()
warnings()
