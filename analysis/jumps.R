#!/usr/bin/env Rscript

source("analysis/utils.R")
source("analysis/analysis.R")

library(dplyr)

analyze_database <- function(database_file_path) {
  components <- stringr::str_split(
    basename(tools::file_path_sans_ext(database_file_path)), "-", 2)[[1]]
  
  db <- src_sqlite(database_file_path)
  jumps <- db %>% tbl("jumps") %>% collect
  
  list(call_depth = jumps %>% 
                    group_by(call_depth) %>% count %>% 
                    as.data.frame,
       promise_depth = jumps %>% 
                       group_by(promise_depth) %>% count %>% 
                       as.data.frame,
       depth = jumps %>% 
               mutate(depth=call_depth + promise_depth) %>% 
               group_by(depth) %>% count %>% 
               as.data.frame,
       restarts = jumps %>% 
                  group_by(restart) %>% count %>% 
                  as.data.frame %>% 
                  mutate(restart=as.logical(restart)))
}

combine_analyses <- function(acc, element) {
  for(name in names(acc)) {    
    acc[[name]] = bind_rows(acc[[name]], element[[name]])  
  }  
  acc
}

summarize_analyses <- function(analyses) {
  list(call_depth = analyses$call_depth %>% 
                    group_by(call_depth) %>% 
                    summarise(number=sum(n)) %>%
                    mutate(percent=(100*number/sum(number))),
       promise_depth = analyses$promise_depth %>% 
                    group_by(promise_depth) %>% 
                    summarise(number=sum(n)) %>%
                    mutate(percent=(100*number/sum(number))),
       depth = analyses$depth %>% 
                    group_by(depth) %>% 
                    summarise(number=sum(n)) %>%
                    mutate(percent=(100*number/sum(number))),
       restarts = analyses$restarts %>% 
                    group_by(restart) %>% 
                    summarise(number=sum(n)) %>%
                    mutate(percent=(100*number/sum(number))))
}

#return a list of ggplot2 objects
visualize_analyses <- function(analyses) {
  list(
    call_depth =
      ggplot(analyses$call_depth, aes(x=call_depth, y=number)) +
      ggtitle("Jump depth by call") +
      geom_col() +
      scale_y_continuous(labels=pp_trunc) +
      theme(legend.position="none", axis.title.x=element_blank()) +
      xlab("Jump depth (measured in calls)"),
    promise_depth =
      ggplot(analyses$promise_depth, aes(x=promise_depth, y=number)) +
      ggtitle("Jump depth by promises") +
      geom_col() +
      scale_y_continuous(labels=pp_trunc) +
      theme(legend.position="none", axis.title.x=element_blank()) +
      xlab("Jump depth (measured in promises)"),
    depth =
      ggplot(analyses$depth, aes(x=depth, y=number)) +
      ggtitle("Jump depth by promises/calls") +
      geom_col() +
      scale_y_continuous(labels=pp_trunc) +
      theme(legend.position="none", axis.title.x=element_blank()) +
      xlab("Jump depth (measured in promises)"),
    restarts = 
      ggplot(analyses$restarts, aes(x=restart, y=number)) +
      ggtitle("Jump restarts") +
      geom_col() +
      scale_y_continuous(labels=pp_trunc) +
      theme(legend.position="none", axis.title.x=element_blank()) +
      xlab("Jump restarted computation")
   )
}

latex_analyses <- function(analyses) {
  list()
}

latex_tables <- function(analyses) {
  list(call_depth =
          analyses$call_depth %>%
          mutate(number=pp_trunc(number), percent=pp_perc(percent)) %>%    
          kable(col.names = c("Call depth", "Number", "Percent"), 
                format="latex"),
       promise_depth = 
          analyses$promise_depth %>%
          mutate(number=pp_trunc(number), percent=pp_perc(percent)) %>%    
          kable(col.names = c("Promise depth", "Number", "Percent"), 
                format="latex"),
       depth =
          analyses$depth %>%
          mutate(number=pp_trunc(number), percent=pp_perc(percent)) %>%    
          kable(col.names = c("Depth", "Number", "Percent"), format="latex"),
      restarts = 
          analyses$restarts %>%
          mutate(number=pp_trunc(number), percent=pp_perc(percent)) %>%    
          kable(col.names = c("Restart", "Number", "Percent"), format="latex"))
}

main <- function() {
  drive_analysis("general-info",
                 analyze_database,
                 combine_analyses,
                 summarize_analyses,
                 export_as_tables,  # exports CSV files automatically
                 import_as_tables,  # (re-)imports data from CSV files
                 visualize_analyses,
                 export_as_images,
                 latex_analyses,
                 export_as_latex_defs,
                 latex_tables,
                 export_as_latex_tables)
}

main()




