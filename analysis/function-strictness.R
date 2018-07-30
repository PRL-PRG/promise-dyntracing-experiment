#!/usr/bin/env Rscript

## WARN - no good way in R to find path of current script
##        perhaps the only good solution is to create a
##        package for the analysis ?
source("analysis/utils.R")
source("analysis/analysis.R")

summarize_analyses <- function(analyses) {

    print(analyses)

    print(analyses$`function-name`)


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

    call_count <-
        analyses$`function-name` %>%
        print() %>%
        group_by(function_id) %>%
        summarize(call_count = sum(as.numeric(count)))

    parameter_usage_distribution <-
        analyses$`function-formal-parameter-usage-count` %>%
        group_by(function_id, formal_parameter_position) %>%
        summarize(evaluation_count = sum(count)) %>%
        ungroup() %>%
        left_join(call_count, by = "function_id")


    parameter_classification <-
        parameter_usage_distribution %>%
        group_by(function_id, formal_parameter_position) %>%
        summarize(classification = if(evaluation_count >= call_count)
                                       "Always Evaluated"
                                   else if(evaluation_count == 0)
                                       "Never Evaluated"
                                   else
                                       "Sometimes Evaluated") %>%
    ungroup() %>%
    group_by(classification) %>%
    summarize(parameter_count = sum(as.numeric(n())))

    total_parameter_count <- sum(parameter_classification$parameter_count)

  parameter_classification <-
      parameter_classification %>%
    mutate(relative_parameter_count =
             parameter_count / total_parameter_count)

  function_classification <-
    parameter_usage_distribution %>%
    group_by(function_id) %>%
    summarize(classification = if(all(evaluation_count >= call_count))
                                 "Always"
                               else if(any(evaluation_count == 0))
                                 "Partial"
                               else
                                 "Sometimes") %>%
    ungroup() %>%
    group_by(classification) %>%
    summarize(function_count = sum(as.numeric(n()))) %>%
    mutate(relative_function_count = function_count / total_function_count)

  list(argument_usage_count = argument_usage_count,
       argument_usage_distribution = argument_usage_distribution,
       function_classification = function_classification,
       parameter_classification = parameter_classification,
       summary = tibble(total_function_count = total_function_count,
                        total_parameter_count = total_parameter_count))
}

visualize_analyses <- function(analyses) {

  total_function_count <- analyses$summary$total_function_count
  total_parameter_count <- analyses$summary$total_parameter_count

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


  parameter_classification <-
    analyses$parameter_classification %>%
    rename("Formal parameter classification" = classification) %>%
    ggplot(aes(`Formal parameter classification`, relative_parameter_count)) +
    geom_col() +
    scale_y_continuous(sec.axis = sec_axis(~ . * total_parameter_count,
                                           labels = count_labels),
                       labels = relative_labels) +
    labs(y = "Formal parameter count (%)",
         title =  "Formal parameter distribution by evaluation mode") +
    scale_fill_gdocs()

  function_classification <-
    analyses$function_classification %>%
    rename("Function classification" = classification) %>%
    ggplot(aes(`Function classification`, relative_function_count)) +
    geom_col() +
    scale_y_continuous(sec.axis = sec_axis(~ . * total_function_count,
                                           labels = count_labels),
                       labels = relative_labels) +
    labs(y = "Function count (%)",
         title =  "Function distribution by evaluation mode") +
    scale_fill_gdocs()


  list(function_strictness_ordering = function_strictness_ordering,
       parameter_classification = parameter_classification,
       function_classification = function_classification)
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
