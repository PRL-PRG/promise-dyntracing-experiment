#!/usr/bin/env Rscript

source("analysis/utils.R")
source("analysis/analysis.R")

library(dplyr)
library(knitr)

summarize_analyses <- function(analyses) {

  total_count <- sum(as.numeric(analyses$`promise-evaluation-distance`$count))

  evaluation_distance <-
    analyses$`promise-evaluation-distance` %>%
    mutate(distance = closure_count + special_count +
             builtin_count + promise_count) %>%
    group_by(promise_type, distance) %>%
    summarize(count = sum(as.numeric(count))) %>%
    ungroup() %>%
    mutate(relative_count = count/total_count,
           promise_type = ifelse(promise_type == "ca",
                                 "Custom argument",
                                 "Default argument"))

  list(evaluation_distance = evaluation_distance,
       total_count = tibble(total_count = total_count))
}

visualize_analyses <- function(analyses) {
  evaluation_distance_visualization <-
    analyses$evaluation_distance %>%
    rename("Promise type" = promise_type) %>%
    ggplot(aes(x=distance, weight=relative_count, fill = `Promise type`)) +
    geom_bar() +
    scale_y_continuous(labels = relative_labels) +
    labs(y ="Count (%)") +
    labs(x = "Distance (number of functions and promises)") +
    scale_fill_gdocs() +
    theme(legend.position = "bottom")

  list(evaluation_distance_visualization = evaluation_distance_visualization)
}

latex_analyses <- function(analyses) {
  list()
}

main <-
  function() {
    analyzer <-
      create_analyzer("Promise evaluation distances",
                      combine_analyses,
                      summarize_analyses,
                      visualize_analyses,
                      latex_analyses)
    drive_analysis(analyzer)
  }

main()
warnings()

