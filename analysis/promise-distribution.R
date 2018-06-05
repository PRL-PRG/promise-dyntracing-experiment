#!/usr/bin/env Rscript

## WARN - no good way in R to find path of current script
##        perhaps the only good solution is to create a
##        package for the analysis ?
source("analysis/utils.R")
source("analysis/analysis.R")

summarize_analyses <- function(analyses) {

  evaluated_promise_count <-
    analyses$`evaluated-promise-type` %>%
    group_by(promise_type) %>%
    summarize(count = sum(as.numeric(count))) %>%
    ungroup() %>%
    mutate(promise_mode = "Evaluated")

  unevaluated_promise_count <-
    analyses$`unevaluated-promise-type` %>%
    group_by(promise_type) %>%
    summarize(count = sum(as.numeric(count))) %>%
    ungroup() %>%
    mutate(promise_mode = "Unevaluated")

  promise_distribution = rbind(evaluated_promise_count,
                               unevaluated_promise_count)

  total_promise_count <- sum(as.numeric(promise_distribution$count))

  promise_distribution <-
    promise_distribution %>%
    mutate(relative_count = count / total_promise_count,
           promise_type = ifelse(promise_type == "ca",
                                 "Custom argument",
                          ifelse(promise_type == "da",
                                 "Default argument",
                                 "Non argument")))

  list("promise_distribution" = promise_distribution)

}

visualize_analyses <- function(analyses) {

  total_object_count <- analyses$aggregate$total_object_count

  promise_distribution <-
    analyses$promise_distribution %>%
    rename("Promise mode" = promise_mode) %>%
    ggplot(aes(promise_type, weight = relative_count, fill=`Promise mode`)) +
    geom_bar() +
    scale_y_continuous(labels = relative_labels) +
    labs(y ="Count (%)") +
    labs(x = "Promise type") +
    guides(title = "Promise distribution") +
    scale_fill_gdocs() +
    theme(legend.position = "bottom")

  list(promise_distribution = promise_distribution)
}

latex_analyses <-
  function(analyses) {
    list()
  }

main <-
  function() {
    analyzer <-
      create_analyzer("Promise Analysis",
                      combine_analyses,
                      summarize_analyses,
                      visualize_analyses,
                      latex_analyses)
    drive_analysis(analyzer)
  }

main()
warnings()
