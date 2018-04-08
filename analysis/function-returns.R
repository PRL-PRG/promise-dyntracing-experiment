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
  data <- 
    db %>% 
    tbl("call_returns") %>% 
    left_join(db %>% tbl("calls") %>% rename(call_id=id) %>% select(call_id, function_id), by="call_id") %>% collect()
  
  list(
    call_return_types = data %>% group_by(return_value_type) %>% count %>% as.data.frame %>% mutate(return_value_type=sapply(return_value_type, typename)),
    separate_function_return_types = data %>% group_by(function_id, return_value_type) %>% count %>% group_by(return_value_type) %>% count %>% as.data.frame %>% mutate(return_value_type=sapply(return_value_type, typename)),
    joint_function_return_types = data %>% 
      group_by(function_id) %>% collect %>% 
      summarize(return_value_types=paste( lapply(unique(sort(return_value_type)), function(x) typename(as.character(x))), collapse = " ")) %>% 
      group_by(return_value_types) %>% count %>% as.data.frame,
    functions_returning_environments = data %>% filter(return_value_type == 4) %>% select(function_id) %>% group_by(function_id) %>% summarise() %>% as.data.frame
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
  list(
    call_return_types = analyses$call_return_types %>% group_by(return_value_type) %>% summarise(number=sum(n)),
    separate_function_return_types = analyses$separate_function_return_types %>% group_by(return_value_type) %>% summarise(number=sum(nn)),
    joint_function_return_types = analyses$joint_function_return_types %>% group_by(return_value_types) %>% summarise(number=sum(n)),
    functions_returning_environments = analyses$functions_returning_environments %>% group_by(function_id) %>% summarise())
}

#return a list of ggplot2 objects
visualize_analyses <- function(analyses) {
  list(
    call_return_types = 
      ggplot(analyses$call_return_types, aes(x=return_value_type, y=number)) +
      ggtitle("Call return types") +
      geom_col() +
      scale_y_continuous(labels=pp_trunc) +
      theme(legend.position="none", axis.title.x=element_blank()) +
      xlab("Return type"),
    separate_function_return_types = 
      ggplot(analyses$separate_function_return_types, aes(x=return_value_type, y=number)) +
      ggtitle("Function return types (separate)") +
      geom_col() +
      scale_y_continuous(labels=pp_trunc) +
      theme(legend.position="none", axis.title.x=element_blank()) +
      xlab("Return type"),
    joint_function_return_types = 
      ggplot(analyses$joint_function_return_types, aes(x=return_value_types, y=number)) +
      ggtitle("Function return types (joint)") +
      geom_col() +
      scale_y_continuous(labels=pp_trunc) +
      theme(legend.position="none", axis.title.x=element_blank()) +
      xlab("Return type")
    )
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




