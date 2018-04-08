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
  jumps <- db %>% tbl("jumps")
  
  list(call_depth=(jumps %>% group_by(call_depth) %>% count %>% as.data.frame), #%>% mutate(db=database_file_path, package = components[1])
       promise_depth=(jumps %>% group_by(promise_depth) %>% count %>% as.data.frame),
       depth=(jumps %>% mutate(depth=call_depth + promise_depth) %>% group_by(depth) %>% count %>% as.data.frame),
       restarts=(jumps %>% group_by(restart) %>% count %>% as.data.frame %>% mutate(restart=as.logical(restart))))
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
  list(call_depth=(analyses$call_depth %>% group_by(call_depth) %>% summarise(n=sum(n))),
       promise_depth=(analyses$promise_depth %>% group_by(promise_depth) %>% summarise(n=sum(n))),
       depth=(analyses$depth %>% group_by(depth) %>% summarise(n=sum(n))),
       restarts=(analyses$restarts %>% group_by(restart) %>% summarise(n=sum(n))))
}

#return a list of ggplot2 objects
visualize_analyses <- function(analyses) {
  list(
    call_depth =
      ggplot(analyses$call_depth, aes(x=call_depth, y=n)) +
      ggtitle("Jump depth by call") +
      geom_col() +
      scale_y_continuous(labels=pp_trunc) +
      theme(legend.position="none", axis.title.x=element_blank()) +
      xlab("Jump depth (measured in calls)"),
    promise_depth =
      ggplot(analyses$promise_depth, aes(x=promise_depth, y=n)) +
      ggtitle("Jump depth by promises") +
      geom_col() +
      scale_y_continuous(labels=pp_trunc) +
      theme(legend.position="none", axis.title.x=element_blank()) +
      xlab("Jump depth (measured in promises)"),
    depth =
      ggplot(analyses$depth, aes(x=depth, y=n)) +
      ggtitle("Jump depth by promises/calls") +
      geom_col() +
      scale_y_continuous(labels=pp_trunc) +
      theme(legend.position="none", axis.title.x=element_blank()) +
      xlab("Jump depth (measured in promises)"),
    restarts = 
      ggplot(analyses$restarts, aes(x=restart, y=n)) +
      ggtitle("Jump restarts") +
      geom_col() +
      scale_y_continuous(labels=pp_trunc) +
      theme(legend.position="none", axis.title.x=element_blank()) +
      xlab("Jump restarted computation")
   )
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




