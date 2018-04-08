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
  
  list(
    specific_calls=data <- db %>% tbl("calls") %>% select(function_name) %>% collect %>% filter(grepl("delayedAssign", function_name)) %>% group_by(function_name) %>% count %>% as.data.frame
  )
}

# combine dataframes from analyzeDatabase
# kind of like fold
# returns a list that has the same shape as analyzeDatabase
combine_analyses <- function(acc, element) {
  for(name in names(acc)) {    
    acc[[name]] = bind_rows(acc[[name]], element[[name]])  
  }  
  acc
}

# REDUCE
# runs analyses on all the data from all the vignettes
# analyses is the list form combine analyses
# returns the summarized data that undergoes visualization
summarize_analyses <- function(analyses) {
  list(specific_calls = analyses$specific_calls %>% group_by(function_name) %>% summarise(number=sum(n)))
}

#return a list of ggplot2 objects
visualize_analyses <- function(analyses) {
  list()
}

main <- function() {  
  drive_analysis("function-returns",
                 analyze_database,
                 combine_analyses,
                 summarize_analyses, 
                 export_as_tables,  # exports CSV files automatically
                 import_as_tables,  # (re-)imports data from CSV files
                 visualize_analyses,
                 export_as_images)
}

main()




