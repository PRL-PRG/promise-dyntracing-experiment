#!/usr/bin/env Rscript

source("analysis/utils.R")
source("analysis/analysis.R")

summarize_analyses <- function(analyses) {

  delayed_assign <-
    analyses$`function-name` %>%
    filter(str_detect(function_name, ".*delayedAssign.*")) %>%
    group_by(package, vignette, function_id, function_name) %>%
    summarize(count = sum(count))

  list(delayed_assign = delayed_assign)
}

visualize_analyses <- function(analyses) {
  list()
}

latex_analyses <- function(analyses) {
  list()
}

main <- function() {
  analyzer <-
    create_analyzer("Delayed Assign Usage Analysis",
                    combine_analyses,
                    summarize_analyses,
                    visualize_analyses,
                    latex_analyses)
  drive_analysis(analyzer)
}

main()
warnings()
