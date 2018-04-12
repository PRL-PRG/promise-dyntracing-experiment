#!/usr/bin/env Rscript

source("analysis/utils.R")
source("analysis/analysis.R")

library(dplyr)

analyze_database <- function(database_file_path) {
  db <- src_sqlite(database_file_path)
  promises <- db %>% tbl("promises")
  print(promises %>% group_by(full_type) %>% count %>% 
                          as.data.frame)
  list(promise_types=promises %>% group_by(type) %>% count %>% as.data.frame, 
       promise_full_types=promises %>% group_by(full_type) %>% count %>% 
                          as.data.frame)
}

combine_analyses <- function(acc, element) {
  for(name in names(acc)) {    
    acc[[name]] = bind_rows(acc[[name]], element[[name]])  
  }  
  acc
}

summarize_analyses <- function(analyses) {
  list(
    promise_types=analyses$promise_types %>% 
                  group_by(type) %>% summarise(number=sum(as.numeric(n))) %>% 
                  ungroup %>% mutate(type=humanize_promise_type(type)),
    
    promise_full_types=analyses$promise_full_types %>% 
                       group_by(full_type) %>% 
                       summarise(number=sum(as.numeric(n))) %>%
                       ungroup %>% 
                       mutate(full_type=humanize_full_promise_type(full_type)))
}

visualize_analyses <- function(analyses) {
  list(
    promise_types =
      ggplot(analyses$promise_types, aes(x=type, y=number)) +
      # ggtitle("Promise types") +
      geom_col() +
      scale_y_continuous(labels=pp_trunc) +
      theme(legend.position="none", axis.title.x=element_blank())
   )
}

latex_analyses <- function(analyses) {
  list()
}

main <- function() {
  drive_analysis("promise-types",
                 analyze_database,
                 combine_analyses,
                 summarize_analyses,
                 export_as_tables,  # exports CSV files automatically
                 import_as_tables,  # (re-)imports data from CSV files
                 visualize_analyses,
                 export_as_images,
                 latex_analyses,
                 export_as_latex_defs)
}

main()




