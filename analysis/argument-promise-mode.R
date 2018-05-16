#!/usr/bin/env Rscript

## TODO - no good way in R to find path of current script
##        perhaps the only good solution is to create a
##        package for the analysis ?
source("analysis/utils.R")
source("analysis/analysis.R")

summarize_analyses <- function(analyses) {
  promise_type_distribution <-
    analyses$`promise-type` %>%
    group_by(promise_type) %>%
    summarize(count = sum(count))

  promise_expression_type_distribution <-
    analyses$`promise-type` %>%
    group_by(promise_type, promise_expression_type) %>%
    summarize(count = sum(count))

  list(promise_type_distribution = promise_type_distribution,
       promise_expression_type_distribution = promise_expression_type_distribution)
}

visualize_analyses <- function(analyses) {

  mode_total <-
    analyses$promise_expression_type_distribution %>%
    ggplot(aes(promise_type, weight = count,
               fill=reorder(promise_expression_type, count))) +
    geom_bar(position="fill") +
    labs(title = "Promise count",
         y = "PROMISE COUNT")

  promise_expression_type <-
    analyses$promise_expression_type_distribution %>%
    mutate(promise_expression_type_classification = classify_type(promise_expression_type)) %>%
    ggplot(aes(promise_expression_type_classification, weight = count, fill=reorder(promise_expression_type, count))) +
    geom_bar(position="stack") +
    ## scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
    ##               labels = trans_format("log10", math_format(10^.x))) +
    theme(axis.text.x=element_text(angle = 90, vjust=0.5)) +
    labs(title = "Promise mode distribution by expression slot type",
         y = "PROMISE COUNT + 1 (log10 scale)")

  mode_expression_type <-
    analyses$promise_expression_type_distribution %>%
    ggplot(aes(promise_expression_type, weight = count, fill=promise_type)) +
    geom_bar(position="fill") +
    ## scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
    ##               labels = trans_format("log10", math_format(10^.x))) +
    theme(axis.text.x=element_text(angle = 90, vjust=0.5)) +
    labs(title = "Promise mode distribution by expression slot type",
         y = "PROMISE COUNT + 1 (log10 scale)")

  list(mode_total = mode_total,
       promise_expression_type = promise_expression_type,
       mode_expression_type = mode_expression_type)

  ## script_count <- length(unique(analyses$promise_mode[["SCRIPT"]]))

  ## mode_average <-
  ##   analyses$promise_mode %>%
  ##   group_by(MODE) %>%
  ##   summarize(COUNT = sum(COUNT) / script_count) %>%
  ##   ggplot(aes(`MODE`, weight = COUNT)) +
  ##   geom_bar() +
  ##   labs(title = "Promise mode average per script",
  ##        y = "AVERAGE PROMISE COUNT")

  ## mode_distribution <-
  ##   analyses$promise_mode %>%
  ##   group_by(SCRIPT, MODE) %>%
  ##   summarize(COUNT = sum(COUNT)) %>%
  ##   ungroup() %>%
  ##   ggplot(aes(`MODE`, COUNT)) +
  ##   geom_violin(na.rm = TRUE, width=1.0) +
  ##   geom_boxplot() +
  ##   labs(title = "Promise mode distribution")

  ## mode_relative_distribution <-
  ##   analyses$promise_mode %>%
  ##   group_by(SCRIPT, MODE) %>%
  ##   summarize(COUNT = sum(COUNT)) %>%
  ##   mutate(COUNT = COUNT/sum(COUNT)) %>%
  ##   ungroup() %>%
  ##   ggplot(aes(MODE, COUNT)) +
  ##   geom_violin(draw_quantiles = c(0.25, 0.5, 0.75), width=1.0) +
  ##   ggtitle("Promise mode distribution (relative)")


  ## mode_value_type <-
  ##   analyses$promise_mode %>%
  ##   filter(SLOT == "VALUE") %>%
  ##   group_by(TYPE, MODE) %>%
  ##   ## added +1 below as it is log scale and
  ##   ## log10(0) = -Inf
  ##   summarize(COUNT = sum(COUNT) + 1) %>%
  ##   ggplot(aes(TYPE, weight = COUNT, fill=MODE)) +
  ##   geom_bar(position="dodge") +
  ##   scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
  ##                 labels = trans_format("log10", math_format(10^.x))) +
  ##   theme(axis.text.x=element_text(angle = 90, vjust=0.5)) +
  ##   labs(title = "Promise mode distribution by value slot type",
  ##        y = "PROMISE COUNT + 1 (log10 scale)")

  ## list("mode_total" = mode_total,
  ##      "mode_average" = mode_average,
  ##      "mode_distribution" = mode_distribution,
  ##      "mode_relative_distribution" = mode_relative_distribution,
  ##      "mode_expression_type" = mode_expression_type,
  ##      "mode_value_type" = mode_value_type)
}

latex_analyses <- function(analyses) {
  list()
}

main <-
  function() {
    analyzer <-
      create_analyzer("Argument Promise Mode",
                      combine_analyses,
                      summarize_analyses,
                      visualize_analyses,
                      latex_analyses)
    drive_analysis(analyzer)
  }

main()
warnings()
