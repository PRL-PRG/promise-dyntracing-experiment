#!/usr/bin/env Rscript

source("analysis/utils.R")
source("analysis/analysis.R")

library(dplyr)
library(knitr)

analyze_database <- function(database_file_path) {
  components <- stringr::str_split(
    basename(tools::file_path_sans_ext(database_file_path)), "-", 2)[[1]]
  
  db <- src_sqlite(database_file_path)
  promise_evaluations <- db %>% 
    tbl("promise_evaluations") %>% 
    filter(event_type == 15) %>%
    select(effective_distance=effective_distance_from_origin, 
           actual_distance=actual_distance_from_origin)

  list(actual_distances = promise_evaluations %>% 
                          group_by(actual_distance) %>%
                          count %>% as.data.frame)
}

summarize_analyses <- function(analyses) {
  data <-
    list(actual_distances = analyses$actual_distances %>% 
                            group_by(actual_distance) %>%
                            summarise(number=sum(as.numeric(n))) %>%
                            mutate(percent=(100*number/sum(number))))
  
  data$actual_distances_short = rbind(data$actual_distances %>%
                                      filter(actual_distance<25) %>%
                                      select(actual_distance, number),
                                      data$actual_distances %>%
                                      filter(actual_distance>=25) %>%
                                      summarise(actual_distance=paste0(25,"+"),
                                                number=sum(number))) %>%
                                      mutate(percent=(100*number/sum(number)),
                                             actual_distance=
                                               ifelse(actual_distance==-1, "E", 
                                                      actual_distance))
  data
}

visualize_analyses <- function(analyses) {
  list(
    actual_distances = 
      ggplot(analyses$actual_distances_short, aes(x=actual_distance, y=number)) +
      geom_col() +
      scale_y_continuous(labels=pp_trunc) +
      scale_x_discrete(limits=c(0:(25-1),paste0(25,"+"), "E")) +
      theme(legend.position="none", axis.title.x=element_blank()))
}

latex_analyses <- function(analyses) {
  list(evaluationDistancesTbl = 
          analyses$actual_distances_short %>%
               mutate(number=pp_trunc(number),
                      percent=pp_perc(percent)) %>%
               kable(col.names = c("Distance", "Number", "Percent"),
                     format = "latex"),
       evaluationDistancesAllTbl = 
          analyses$actual_distances_short %>%
               mutate(number=pp_trunc(number),
                      percent=pp_perc(percent)) %>%
               kable(col.names = c("Distance", "Number", "Percent"), 
                     format = "latex"))
}

main <-
  function() {
    analyzer <-
      create_analyzer("Promise evaluation distances",
                      analyze_database,
                      combine_analyses,
                      summarize_analyses,
                      visualize_analyses,
                      latex_analyses)
    drive_analysis(analyzer)
  }

main()
warnings()

