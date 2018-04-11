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
    tbl("promise_lifecycle") %>%
    select(promise_id, event_type, inside_force) %>%
    group_by(promise_id) %>%
    summarise(
      forces = sum(event_type == 1),
      lookups = sum(event_type == 5 & inside_force == 1),
      metaprogramming_any = sum(event_type >= 3) & sum(event_type <= 8) & (inside_force != 1),
      metaprogramming_set = sum(event_type >= 6) & sum(event_type <= 8) & (inside_force != 1)
    ) %>%
    mutate(
      classification = ifelse(forces > 0,
                       ifelse(forces > 1, "multiforce",
                       ifelse(lookups > 0, "force and reuse", "just force")),
                       "unforced"),
      metaprogramming = ifelse(metaprogramming_any > 0,
                        ifelse(metaprogramming_set > 0, "meta/set", "meta/get"), "clean")
    ) %>%
    group_by(classification, metaprogramming) %>%
    count %>%
    as.data.frame

  list(accesses=data)
}

# REDUCE
# runs analyses on all the data from all the vignettes
# analyses is the list form combine analyses
# returns the summarized data that undergoes visualization
summarize_analyses <- function(analyses) {

  list(
    accesses=analyses$accesses %>%
      group_by(classification) %>%
      summarise(number=sum(n)) %>%
      mutate(percent=(100*number/sum(number))),

    metaprogramming=analyses$accesses %>%
      group_by(metaprogramming) %>%
      summarise(number=sum(n)) %>%
      mutate(percent=(100*number/sum(number))),

    accesses_and_metaprogramming=analyses$accesses %>%
      mutate(label=paste0(classification, " ", metaprogramming)) %>%
      group_by(label) %>%
      summarise(number=sum(n)) %>%
      mutate(percent=(100*number/sum(number))))
}

#return a list of ggplot2 objects
visualize_analyses <- function(analyses) {
  list(
    accesses=
      ggplot(analyses$accesses, aes(x=classification, y=number)) +
      ggtitle("Accesses") +
      geom_col() +
      scale_y_continuous(labels=pp_trunc) +
      theme(legend.position="none", axis.title.x=element_blank()) +
      xlab("Classification"),

    metaprogramming=
      ggplot(analyses$metaprogramming, aes(x=metaprogramming, y=number)) +
      ggtitle("Metaprogramming") +
      geom_col() +
      scale_y_continuous(labels=pp_trunc) +
      theme(legend.position="none", axis.title.x=element_blank()) +
      xlab("Classification"),

    accesses_and_metaprogramming=
      ggplot(analyses$accesses_and_metaprogramming, aes(x=label, y=number)) +
      ggtitle("Accesses/metaprogramming") +
      geom_col() +
      scale_y_continuous(labels=pp_trunc) +
      theme(legend.position="none", axis.title.x=element_blank()) +
      xlab("Classification"))
}

latex_analyses <- function(analyses) {
  list()
}

main <- function() {

  drive_analysis("General Info",
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
