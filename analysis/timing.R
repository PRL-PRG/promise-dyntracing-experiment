#!/usr/bin/env Rscript

source("analysis/utils.R")
source("analysis/analysis.R")

library(dplyr)

analyze_database <- function(database_file_path) {
  components <- stringr::str_split(
    basename(tools::file_path_sans_ext(database_file_path)), "-", 2)[[1]]
  
  trim <- function(x) 
    sub("TIMING_", "", sub("OCCURANCE_", "", sub("PROBE_", "", x, fixed=T), fixed=T), fixed=T)

  db <- src_sqlite(database_file_path)
  probe_and_timing_and_occurance_table <-
    db %>% tbl("metadata") %>% collect %>% 
    mutate(occurance=grepl('^OCCURANCE', key)) %>%
    mutate(timing=(grepl('^TIMING', key) | grepl('^PROBE', key))) %>%
    filter(timing | occurance) %>%
    # mutate(segment=ifelse(timing|occurance, "segment", "probe"),
    #        measurement=ifelse(timing|probe, "timing", "occurance")) %>%
    mutate(segment=sapply(str_split(mapply(trim, key), "/"),`[`, 2),
           probe=sapply(str_split(mapply(trim, key), "/"),`[`, 1)) %>%
    mutate(segment=ifelse(is.na(segment), "TOTAL", segment)) %>%
    select(-key)

  list(timing=probe_and_timing_and_occurance_table)
}

summarize_analyses <- function(analyses) {
  timing <- 
    analyses$timing %>% 
    group_by(probe, segment) %>% 
    summarize(time=sum(as.numeric(ifelse(timing, value, 0))), 
              count=sum(as.numeric(ifelse(occurance, value, 0)))) %>% 
    mutate(count=ifelse(count==0, 1, count)) %>%
    mutate(number=time/count) %>% 
    select(probe, segment, number) %>% 
    ungroup

  list(all=timing, 
    total=timing %>% 
      filter(grepl("TOTAL", segment)) %>% select(-segment) %>%
      mutate(percent=(100*number/sum(number))), 
    probes_and_segments=timing %>% 
      filter(!grepl("TOTAL", segment)) %>%
      mutate(percent=(100*number/sum(number))),
    segments=timing %>% 
      filter(!grepl("TOTAL", segment)) %>%
      group_by(segment) %>%
      summarize(number=sum(number)) %>%
      ungroup %>%
      mutate(percent=(100*number/sum(number))),
    probes=timing %>% 
      filter(!grepl("TOTAL", segment)) %>%
      group_by(probe) %>%
      summarize(number=sum(number)) %>%
      ungroup %>%
      mutate(percent=(100*number/sum(number)))
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
