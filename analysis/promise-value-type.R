#!/usr/bin/env Rscript

## WARN - no good way in R to find path of current script
##        perhaps the only good solution is to create a
##        package for the analysis ?
source("analysis/utils.R")
source("analysis/analysis.R")

summarize_analyses <- function(analyses) {
  evaluated_promise_type <-
    analyses$`evaluated-promise-type` %>%
    select(promise_type, promise_value_type, count) %>%
    group_by(promise_type, promise_value_type) %>%
    summarize(count = sum(as.numeric(count))) %>%
    ungroup()

  total_count <- sum(as.numeric(evaluated_promise_type$count))

  evaluated_promise_type <-
    evaluated_promise_type %>%
    mutate(relative_count = count / total_count,
           promise_type = ifelse(promise_type == "ca",
                                 "Custom argument",
                          ifelse(promise_type == "na",
                                 "Non argument",
                                 "Default argument")))
    list("evaluated_promise_type" = evaluated_promise_type)
}

visualize_analyses <- function(analyses) {

  promise_type_count_by_value_type <-
    analyses$evaluated_promise_type %>%
    rename("Promise type" = promise_type) %>%
    ggplot(aes(promise_value_type,
               weight = relative_count,
               fill=`Promise type`)) +
    geom_bar() +
    scale_y_continuous(labels = relative_labels) +
    labs(x = "Value type",
         y ="Count (%)",
         title = "Evaluated promise value type") +
    guides(title = NULL) +
    scale_fill_gdocs() +
    theme(legend.position = "bottom",
          axis.text.x = element_text(angle=60, hjust=1))


  value_type_count_by_promise_type <-
    analyses$evaluated_promise_type %>%
    rename("Promise type" = promise_type) %>%
    ggplot(aes(`Promise type`,
               weight = relative_count,
               fill=promise_value_type)) +
    geom_bar(position="fill") +
    scale_y_continuous(labels = relative_labels) +
    labs(x = "Promise type",
         y ="Count (%)",
         title = "Evaluated promise value type") +
    guides(title = NULL) +
    scale_fill_gdocs() +
    theme(legend.position = "bottom")

  list("promise_type_count_by_value_type" = promise_type_count_by_value_type,
       "value_type_count_by_promise_type" = value_type_count_by_promise_type)

}

latex_analyses <-
  function(analyses) {
    list()
  }

main <-
  function() {
    analyzer <-
      create_analyzer("Promise Value Type Analysis",
                      combine_analyses,
                      summarize_analyses,
                      visualize_analyses,
                      latex_analyses)
    drive_analysis(analyzer)
  }

main()
warnings()
