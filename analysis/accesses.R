#!/usr/bin/env Rscript

source("analysis/utils.R")
source("analysis/analysis.R")

suppressPackageStartupMessages(library(dplyr))

humanize_classification <- function(vector) 
  ifelse(vector == 1, "multiforce",
  ifelse(vector == 2, "force and reuse",
  ifelse(vector == 3, "just force",
  ifelse(vector == 4, "unforced", "?"))))

to_bits <- function(x) lapply(x, to_bits_single)

humanize_metaprogramming <- function(vector)
  sapply(vector, function(x) {
    e <- as.numeric(intToBits(x))
    paste(
      Filter(function(x) x!="",
             ifelse(sum(e[1:6]),
                   c(ifelse(sum(e[1:3]), "lookup", ""),
                      ifelse(e[1], "expr", ""),
                      ifelse(e[2], "env", ""),
                      ifelse(e[3], "val", ""),
                      ifelse(sum(e[4:6]), "set", ""),
                      ifelse(e[4], "expr", ""),
                      ifelse(e[5], "env", ""),
                      ifelse(e[6], "val", "")),
                    c("clean"))), 
             collapse=" ")})

analyze_database <- function(database_file_path) {
  components <- stringr::str_split(
    basename(tools::file_path_sans_ext(database_file_path)), "-", 2)[[1]]

  db <- src_sqlite(database_file_path)

  data <-
    db %>%
    tbl("promise_lifecycle") %>%
    select(promise_id, event_type, inside_force) %>%
    group_by(promise_id) %>%
    summarise(
      forces =              sum(event_type == 1, na.rm = TRUE),
      lookups =             sum(event_type == 5, na.rm = TRUE) & 
                            (inside_force == 1),
      metaprogramming_any = sum(event_type >= 3, na.rm = TRUE) & 
                            sum(event_type <= 8, na.rm = TRUE) & 
                            (inside_force != 1),
      lookup_expr =         sum(event_type == 3, na.rm = TRUE) & 
                            (inside_force != 1),
      lookup_env =          sum(event_type == 4, na.rm = TRUE) & 
                            (inside_force != 1),
      lookup_val =          sum(event_type == 5, na.rm = TRUE) & 
                            (inside_force != 1),
      set_expr =            sum(event_type == 6, na.rm = TRUE) & 
                            (inside_force != 1),
      set_env =             sum(event_type == 7, na.rm = TRUE) & 
                            (inside_force != 1),
      set_val =             sum(event_type == 8, na.rm = TRUE) & 
                            (inside_force != 1)) %>%
    mutate(
      classification =      ifelse(forces > 0,
                            ifelse(forces > 1, 1, 
                            ifelse(lookups > 0, 2, 3)), 4),
      metaprogramming =     ifelse(lookup_expr > 0, 1, 0) + 
                            ifelse(lookup_env > 0, 2, 0) +
                            ifelse(lookup_val > 0, 4, 0) + 
                            ifelse(set_expr > 0, 8, 0) + 
                            ifelse(set_env > 0, 16, 0) +
                            ifelse(set_val > 0, 32, 0)) %>%
    group_by(classification, metaprogramming) %>%
    count %>%
    as.data.frame

  list(accesses=data)
}

summarize_analyses <- function(analyses) {
  list(
    accesses=analyses$accesses %>%
      group_by(classification) %>%
      summarise(number=sum(n)) %>%
      mutate(percent=(100*number/sum(number))) %>%
      mutate(classification=humanize_classification(classification)),

    metaprogramming=analyses$accesses %>%
      group_by(metaprogramming) %>%
      summarise(number=sum(n)) %>%
      mutate(percent=(100*number/sum(number))) %>%
      ungroup %>%
      mutate(metaprogramming=humanize_metaprogramming(metaprogramming)),

    accesses_and_metaprogramming=analyses$accesses %>%
      group_by(classification, metaprogramming) %>%
      summarise(number=sum(n)) %>%
      mutate(percent=(100*number/sum(number))) %>%
      ungroup %>%
      mutate(label=paste0(
        classification=humanize_classification(classification), " ",
        metaprogramming=humanize_metaprogramming(metaprogramming))) %>%
      select(label, number, percent)
    )
}

visualize_analyses <- function(analyses) {
  list(
    accesses=
      ggplot(analyses$accesses, aes(x=classification, y=number)) +
      #ggtitle("Accesses") +
      geom_col() +
      scale_y_continuous(labels=count_labels) +
      theme(legend.position="none", axis.title.x=element_blank()),
      #xlab("Access pattern"),

    metaprogramming=
      ggplot(analyses$metaprogramming, aes(x=metaprogramming, y=number)) +
      #ggtitle("Metaprogramming") +
      geom_col() +
      scale_y_continuous(labels=count_labels) +
      theme(legend.position="none", axis.title.x=element_blank()),
      #xlab(""),

    accesses_and_metaprogramming=
      ggplot(analyses$accesses_and_metaprogramming, aes(x=label, y=number)) +
      #ggtitle("Accesses/metaprogramming") +
      geom_col() +
      scale_y_continuous(labels=count_labels) +
      theme(legend.position="none", axis.title.x=element_blank()))
      #xlab("Classification"))

}

latex_analyses <- function(analyses) {
  list()
}

main <-
  function() {
    analyzer <-
      create_analyzer("Promise Metaprogramming and Accesses",
                      analyze_database,
                      combine_analyses,
                      summarize_analyses,
                      visualize_analyses,
                      latex_analyses)
    drive_analysis(analyzer)
  }

main()
warnings()
