#!/usr/bin/env Rscript

## WARN - no good way in R to find path of current script
##        perhaps the only good solution is to create a
##        package for the analysis ?
source("analysis/utils.R")
source("analysis/analysis.R")

summarize_analyses <- function(analyses) {

  argument_usage_count <-
    analyses$`function-formal-parameter-usage-order` %>%
    group_by(function_id) %>%
    summarize(order_count = length(unique(formal_parameter_position_usage_order)))

  total_function_count <- length(argument_usage_count$function_id)

  argument_usage_distribution <-
    argument_usage_count %>%
    group_by(order_count) %>%
    summarize(function_count = length(function_id))

  argument_usage_distribution <-
    argument_usage_distribution %>%
    mutate(relative_function_count = function_count / total_function_count)

  list(argument_usage_count = argument_usage_count,
       argument_usage_distribution = argument_usage_distribution,
       summary = tibble(total_function_count = total_function_count))
}

visualize_analyses <- function(analyses) {

  total_function_count <- analyses$summary$total_function_count

  function_strictness_ordering <-
    analyses$argument_usage_distribution %>%
    rename("Argument Evaluation Orders" = order_count) %>%
    ggplot(aes(`Argument Evaluation Orders`, function_count)) +
    geom_col() +
    scale_y_continuous(sec.axis = sec_axis(~ . / total_function_count,
                                           labels = relative_labels),
                       labels = count_labels) +
    labs(y = "Function count",
         title =  "Function count per argument evaluation order") +
    scale_fill_gdocs()

  list(function_strictness_ordering = function_strictness_ordering)
}

latex_analyses <- function(analyses) {
    list()
}

main <- function() {
    analyzer <-
      create_analyzer("Function Strictness Analysis",
                      combine_analyses,
                      summarize_analyses,
                      visualize_analyses,
                      latex_analyses)
    drive_analysis(analyzer)
}

main()
warnings()
