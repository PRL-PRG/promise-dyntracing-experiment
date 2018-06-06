#!/usr/bin/env Rscript

source("analysis/utils.R")
source("analysis/analysis.R")

summarize_analyses <- function(analyses) {

  function_type <-
    analyses$`function-type` %>%
    group_by(function_id) %>%
    summarize(function_type = first(function_type))

  function_distribution <-
    analyses$`function-name` %>%
    group_by(function_id) %>%
    summarize(count = sum(as.numeric(count))) %>%
    left_join(function_type, by = "function_id")

  count_by_type <-
    function_distribution %>%
    group_by(function_type) %>%
    summarize(call_count = sum(as.numeric(count)),
              function_count = n()) %>%
    ungroup() %>%
    mutate(relative_call_count = call_count/sum(as.numeric(call_count)))

  list(function_distribution = function_distribution,
       count_by_type = count_by_type)
}

visualize_analyses <- function(analyses) {

  count_by_type <-
    analyses$count_by_type %>%
    mutate(function_type = ifelse(function_type == "cl",
                                  "Closure",
                           ifelse(function_type == "bu",
                                  "Builtin",
                                  "Special")))

  total_call_count <- sum(as.numeric(analyses$count_by_type$call_count))

   call_count <-
    count_by_type %>%
    ggplot(aes(function_type, weight = relative_call_count)) +
    geom_bar() +
    scale_y_continuous(sec.axis = sec_axis(~ . * total_call_count,
                                           labels = count_labels),
                       labels = relative_labels) +
    labs(y ="Count (%)",
         x = "Function Type",
         title = "Call Distribution") +
    scale_fill_gdocs()


  function_count <-
    count_by_type %>%
    ggplot(aes(function_type, weight = function_count)) +
    geom_bar() +
    scale_y_continuous(labels = count_labels) +
    labs(y ="Count",
         x = "Function Type",
         title = "Function distribution") +
    scale_fill_gdocs()

  list(call_count = call_count,
       function_count = function_count)
}

latex_analyses <- function(analyses) {
}

main <- function() {
    analyzer <-
      create_analyzer("Function Distribution",
                      combine_analyses,
                      summarize_analyses,
                      visualize_analyses,
                      latex_analyses)
    drive_analysis(analyzer)
}

main()
warnings()
