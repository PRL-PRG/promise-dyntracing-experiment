#!/usr/bin/env Rscript

## WARN - no good way in R to find path of current script
##        perhaps the only good solution is to create a
##        package for the analysis ?
source("analysis/utils.R")
source("analysis/analysis.R")

summarize_analyses <- function(analyses) {

  function_return_type_distribution <-
    analyses$`function-return-type` %>%
    group_by(return_type, function_id) %>%
    summarize(call_count = sum(as.numeric(count))) %>%
    ungroup()

  function_return_type_summary <-
    function_return_type_distribution %>%
    group_by(return_type) %>%
    summarize(function_count = n(),
              call_count = sum(as.numeric(call_count)))

  total_call_count <- sum(as.numeric(function_return_type_summary$call_count))

  function_return_type_summary <-
    function_return_type_summary %>%
    mutate(relative_call_count = call_count / total_call_count)


  list(function_return_type_distribution = function_return_type_distribution,
       function_return_type_summary = function_return_type_summary,
       total_call_count = tibble(total_call_count = total_call_count))

}

visualize_analyses <- function(analyses) {

  total_call_count <- analyses$total_call_count$total_call_count

  function_return_type_distribution <-
    analyses$function_return_type_summary %>%
    ggplot(aes(return_type, weight = function_count)) +
    geom_bar() +
    scale_y_continuous(labels = count_labels) +
    labs(y ="Function count",
         x = "Return type",
         title = "Function Count by return type") +
    scale_fill_gdocs() +
    theme(axis.text.x = element_text(angle=60, hjust=1))

  call_return_type_distribution <-
    analyses$function_return_type_summary %>%
    ggplot(aes(return_type, weight = relative_call_count)) +
    geom_bar() +
    scale_y_continuous(sec.axis = sec_axis(~ . * total_call_count,
                                           labels = count_labels),
                       labels = relative_labels) +
    labs(y ="Call count (%)",
         x = "Return type",
         title = "Call count by return type") +
    scale_fill_gdocs() +
    theme(axis.text.x = element_text(angle=60, hjust=1))

  list(call_return_type_distribution = call_return_type_distribution,
       function_return_type_distribution = function_return_type_distribution)
}

latex_analyses <- function(analyses) {

}

main <- function() {
  analyzer <-
    create_analyzer("Function Return Type Analysis",
                    combine_analyses,
                    summarize_analyses,
                    visualize_analyses,
                    latex_analyses)
  drive_analysis(analyzer)
}

main()
warnings()

