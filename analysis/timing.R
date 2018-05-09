#!/usr/bin/env Rscript

source("analysis/utils.R")
source("analysis/analysis.R")

library(dplyr)

analyze_database <- function(database_file_path) {
  db <- src_sqlite(database_file_path)

  list(timing=
      db %>% tbl("metadata") %>% collect %>% 
      mutate(timer=grepl('^TIMER_', key)) %>%
      filter(timer) %>%
      mutate(timer=sapply(str_split(key, "_", n=3), `[`, 2),
             segment=sapply(str_split(key, "_", n=3), `[`, 3),
             time=as.numeric(sapply(str_split(value, "/", n=2), `[`, 1)),
             count=as.numeric(sapply(str_split(value, "/", n=2), `[`, 2))) %>%
      mutate(probe=sapply(str_split(segment, "/", n=3), `[`, 1),
             sub_segment=sapply(str_split(segment, "/", n=3), `[`, 3),
             segment=sapply(str_split(segment, "/", n=3), `[`, 2)) %>%
      select(timer, probe, segment, sub_segment, time, count)
  )
}

summarize_analyses <- function(analyses) {
  timing <- 
    analyses$timing %>% 
    group_by(timer, probe, segment, sub_segment) %>% 
    summarize(time=sum(time), count=sum(count)) %>% 
    ungroup %>%
    mutate(percent=time/sum(time)) %>% 
    select(timer, probe, segment, sub_segment, time, percent, count)

  list(
    all=timing %>% 
      filter(timer=="MAIN"), 
    
    probes=timing %>% 
      filter(timer=="MAIN") %>%
      group_by(probe) %>%
      summarize(time=sum(time), count=max(count)) %>%
      mutate(percent=(100*time/sum(time))) %>%
      arrange(desc(time)), 
    
    segments=timing %>% 
      filter(timer=="MAIN") %>%
      group_by(segment) %>%
      summarize(time=sum(time), count=max(count)) %>%
      mutate(percent=(100*time/sum(time))) %>%
      arrange(desc(time)),
    
    sql=timing %>% 
      filter(timer=="SQL") %>%
      #summarize(time=sum(time), count=max(count)) %>%
      mutate(percent=(100*time/sum(time))) %>%
      arrange(desc(time)),
    
    recorder=timing %>% 
      filter(timer=="RECORDER") %>%
      summarize(time=sum(time), count=max(count)) %>%
      mutate(percent=(100*time/sum(time))) %>%
      arrange(desc(time))
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
      create_analyzer("Timing Information",
                      analyze_database,
                      combine_analyses,
                      summarize_analyses,
                      visualize_analyses,
                      latex_analyses)
    drive_analysis(analyzer)
  }

main()
warnings()
