#!/usr/bin/env Rscript

## WARN - no good way in R to find path of current script
##        perhaps the only good solution is to create a
##        package for the analysis ?
source("analysis/utils.R")
source("analysis/analysis.R")

summarize_analyses <- function(analyses) {
  evaluated_promise_type <-
    analyses$`evaluated-promise-type` %>%
    select(promise_type, promise_expression_type, count) %>%
    mutate(evaluated = "E")

  unevaluated_promise_type <-
    analyses$`unevaluated-promise-type` %>%
    select(promise_type, promise_expression_type, count) %>%
    mutate(evaluated = "U")

  promise_type <-
    evaluated_promise_type %>%
    rbind(unevaluated_promise_type) %>%
    group_by(promise_type, promise_expression_type, evaluated) %>%
    summarize(count = sum(as.numeric(count))) %>%
    ungroup()

  total_count <- sum(as.numeric(promise_type$count))

  promise_type <-
    promise_type %>%
    mutate(promise_type = ifelse(promise_type == "ca",
                                 "Custom argument",
                          ifelse(promise_type == "na",
                                 "Non argument",
                                 "Default argument")),
           evaluated = ifelse(evaluated == "E",
                              "Evaluated",
                              "Unevaluated")) %>%
    spread(evaluated, count, fill = 0) %>%
    mutate(All = Evaluated + Unevaluated)

    list("promise_type" = promise_type)
}

visualize_analyses <- function(analyses) {

  promise_type <-
    analyses$promise_type %>%
    rename("Promise type" = promise_type)

  evaluated_promise_count <- sum(as.numeric(promise_type$Evaluated))
  unevaluated_promise_count <- sum(as.numeric(promise_type$Unevaluated))
  all_promise_count <- sum(as.numeric(promise_type$All))

  promise_type <-
    promise_type %>%
    mutate(Evaluated = Evaluated / evaluated_promise_count,
           Unevaluated = Unevaluated / unevaluated_promise_count,
           All = All / all_promise_count)

  evaluated_promise_expression_visualization <-
    promise_type %>%
    ggplot(aes(promise_expression_type,
               weight = Evaluated,
               fill=`Promise type`)) +
    geom_bar() +
    scale_y_continuous(labels = relative_labels) +
    labs(x = "Expression type",
         y ="Count (%)",
         title = "Evaluated promise expression type") +
    guides(title = NULL) +
    scale_fill_gdocs() +
    theme(legend.position = "bottom",
          axis.text.x = element_text(angle=60, hjust=1))

  unevaluated_promise_expression_visualization <-
    promise_type %>%
    ggplot(aes(promise_expression_type,
               weight = Unevaluated,
               fill=`Promise type`)) +
    geom_bar() +
    scale_y_continuous(labels = relative_labels) +
    labs(x = "Expression type",
         y ="Count (%)",
         title = "Unevaluated promise expression type") +
    guides(title = NULL) +
    scale_fill_gdocs() +
    theme(legend.position = "bottom",
          axis.text.x = element_text(angle=60, hjust=1))

  all_promise_expression_visualization <-
    promise_type %>%
    ggplot(aes(promise_expression_type,
               weight = All,
               fill=`Promise type`)) +
    geom_bar() +
    scale_y_continuous(labels = relative_labels) +
        labs(x = "Expression type",
         y ="Count (%)",
         title = "All promise expression type") +
    guides(title = NULL) +
    scale_fill_gdocs() +
    theme(legend.position = "bottom",
          axis.text.x = element_text(angle=60, hjust=1))

  list("evaluated_promise_expression_visualization" = evaluated_promise_expression_visualization,
       "unevaluated_promise_expression_visualization" = unevaluated_promise_expression_visualization,
       "all_promise_expression_visualization" = all_promise_expression_visualization)

}

latex_analyses <-
  function(analyses) {
    list()
  }

main <-
  function() {
    analyzer <-
      create_analyzer("Promise Expression Type Analysis",
                      combine_analyses,
                      summarize_analyses,
                      visualize_analyses,
                      latex_analyses)
    drive_analysis(analyzer)
  }

main()
warnings()
