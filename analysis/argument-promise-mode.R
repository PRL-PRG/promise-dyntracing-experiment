#!/usr/bin/env Rscript

## TODO - no good way in R to find path of current script
##        perhaps the only good solution is to create a
##        package for the analysis ?
source("analysis/utils.R")
source("analysis/analysis.R")

analyze_database <- function(database_filepath) {

  db <- src_sqlite(database_filepath)
  promises <- tbl(db, "promises")
  promise_associations <- tbl(db, "promise_associations")
  promise_returns <- tbl(db, "promise_returns")
  promise_argument_types <- tbl(db, "promise_argument_types")

  ## if no promises exist, then this script should
  ## be ignored as the analysis cannot be attempted
  if(nrow(collect(promises)) == 0) return(NULL)

  promises <-
    promise_associations %>%
    filter(promise_id >= 0) %>%
    left_join(promises, by = c("promise_id" = "id")) %>%
    select(id = promise_id, full_type) %>%
    collect() %>%
    mutate(full_type = full_type_to_final_type(full_type)) #tail(str_split(full_type, ",")[[1]], n = 1))

  promise_mode <-
    promises %>%
    select(id, full_type) %>%
    rename(EXPRESSION = full_type) %>%
    left_join(promise_returns, by = c("id" = "promise_id"), copy = TRUE) %>%
    rename(VALUE = type) %>%
    left_join(promise_argument_types, by = c("id" = "promise_id"), copy = TRUE) %>%
    select(`PROMISE ID` = id, `EXPRESSION`, `VALUE`, MODE = default_argument) %>%
    collect() %>%
    gather(SLOT, TYPE, -MODE, -`PROMISE ID`) %>%
    group_by(MODE, SLOT, TYPE) %>%
    summarize(COUNT = n()) %>%
    ungroup() %>%
    rowwise() %>%
    mutate(MODE = modename(MODE), TYPE = typename(TYPE)) %>%
    add_column(SCRIPT = basename(file_path_sans_ext(database_filepath)), .before = 1)

  list("promise_mode" = promise_mode)
}

summarize_analyses <- function(analyses) analyses

visualize_analyses <- function(analyses) {

  mode_total <-
    analyses$promise_mode %>%
    group_by(MODE) %>%
    summarize(COUNT = sum(COUNT)) %>%
    ggplot(aes(`MODE`, weight = COUNT)) +
    geom_bar() +
    labs(title = "Promise mode count",
         y = "PROMISE COUNT")

  script_count <- length(unique(analyses$promise_mode[["SCRIPT"]]))

  mode_average <-
    analyses$promise_mode %>%
    group_by(MODE) %>%
    summarize(COUNT = sum(COUNT) / script_count) %>%
    ggplot(aes(`MODE`, weight = COUNT)) +
    geom_bar() +
    labs(title = "Promise mode average per script",
         y = "AVERAGE PROMISE COUNT")

  mode_distribution <-
    analyses$promise_mode %>%
    group_by(SCRIPT, MODE) %>%
    summarize(COUNT = sum(COUNT)) %>%
    ungroup() %>%
    ggplot(aes(`MODE`, COUNT)) +
    geom_violin(na.rm = TRUE, width=1.0) +
    geom_boxplot() +
    labs(title = "Promise mode distribution")

  mode_relative_distribution <-
    analyses$promise_mode %>%
    group_by(SCRIPT, MODE) %>%
    summarize(COUNT = sum(COUNT)) %>%
    mutate(COUNT = COUNT/sum(COUNT)) %>%
    ungroup() %>%
    ggplot(aes(MODE, COUNT)) +
    geom_violin(draw_quantiles = c(0.25, 0.5, 0.75), width=1.0) +
    ggtitle("Promise mode distribution (relative)")

  mode_expression_type <-
    analyses$promise_mode %>%
    filter(SLOT == "EXPRESSION") %>%
    group_by(TYPE, MODE) %>%
    ## added +1 below as it is log scale and
    ## log10(0) = -Inf
    summarize(COUNT = sum(COUNT) + 1) %>%
    ggplot(aes(TYPE, weight = COUNT, fill=MODE)) +
    geom_bar(position="dodge") +
    scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                  labels = trans_format("log10", math_format(10^.x))) +
    theme(axis.text.x=element_text(angle = 90, vjust=0.5)) +
    labs(title = "Promise mode distribution by expression slot type",
         y = "PROMISE COUNT + 1 (log10 scale)")

  mode_value_type <-
    analyses$promise_mode %>%
    filter(SLOT == "VALUE") %>%
    group_by(TYPE, MODE) %>%
    ## added +1 below as it is log scale and
    ## log10(0) = -Inf
    summarize(COUNT = sum(COUNT) + 1) %>%
    ggplot(aes(TYPE, weight = COUNT, fill=MODE)) +
    geom_bar(position="dodge") +
    scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                  labels = trans_format("log10", math_format(10^.x))) +
    theme(axis.text.x=element_text(angle = 90, vjust=0.5)) +
    labs(title = "Promise mode distribution by value slot type",
         y = "PROMISE COUNT + 1 (log10 scale)")

  list("mode_total" = mode_total,
       "mode_average" = mode_average,
       "mode_distribution" = mode_distribution,
       "mode_relative_distribution" = mode_relative_distribution,
       "mode_expression_type" = mode_expression_type,
       "mode_value_type" = mode_value_type)
}

latex_analyses <- function(analyses) {
  list()
}

latex_tables <- function(analyses) {
  list()
}

main <- function() {
  drive_analysis("Argument Mode Analysis",
                 analyze_database,
                 combine_analyses,
                 summarize_analyses,
                 export_as_tables,
                 import_as_tables,
                 visualize_analyses,
                 export_as_images,
                 latex_analyses,
                 export_as_latex_defs,
                 latex_tables,
                 export_as_latex_tables)
}

main()
