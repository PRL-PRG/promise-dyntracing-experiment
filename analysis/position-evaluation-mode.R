#!/usr/bin/env Rscript

## TODO - no good way in R to find path of current script
##        perhaps the only good solution is to create a
##        package for the analysis ?
source("analysis/utils.R")
source("analysis/analysis.R")

analyze_database <- function(database_filepath) {

  db <- src_sqlite(database_filepath)

  arguments_tbl <-
      tbl(db, "arguments") %>%
      select(id, position)

  promise_evaluations_tbl <-
    tbl(db, "promise_evaluations") %>%
    select(promise_id, in_call_id)

  promise_associations_tbl <-
    tbl(db, "promise_associations")

  calls_tbl <-
    tbl(db, "calls") %>%
    select(id, function_id)

  functions_tbl <-
    tbl(db, "functions") %>%
    select(id, type)

  evaluation_mode <-
    promise_associations_tbl %>%
    left_join(promise_evaluations_tbl, by = c("promise_id" = "promise_id")) %>%
    left_join(arguments_tbl, by = c("argument_id" = "id")) %>%
    left_join(calls_tbl, by = c("call_id" = "id")) %>%
    left_join(functions_tbl, by = c("function_id" = "id")) %>%
    select(-promise_id) %>%
    collect() %>%
    rename(`FUNCTION ID` = function_id, `ARGUMENT POSITION`=position) %>%
    group_by(`FUNCTION ID` , `ARGUMENT POSITION`) %>%
    summarize(`STRICT` = sum(!is.na(in_call_id)),
              `LAZY` = sum(is.na(in_call_id))) %>%
    ungroup() %>%
    mutate(`SCRIPT`=basename(file_path_sans_ext(database_filepath)))


  list("evaluation_mode" = evaluation_mode)
}

combine_analyses <- function(acc, element) {
  for(name in names(acc)) {
    acc[[name]] = bind_rows(acc[[name]], element[[name]])
  }
  acc
}

summarize_analyses <- function(analyses) {
  evaluation_mode <-
    analyses$evaluation_mode %>%
    group_by(`FUNCTION ID` , `ARGUMENT POSITION`) %>%
    summarize(`STRICT` = sum(STRICT), `LAZY` = sum(LAZY)) %>%
    rowwise() %>%
    mutate(`EVALUATION MODE` = evaluation_modename(STRICT, LAZY))

  list("evaluation_mode" = evaluation_mode)
}


visualize_analyses <- function(analyses) {
  evaluation_mode_distribution <-
    analyses$evaluation_mode %>%
    print() %>%
    ggplot(aes(`EVALUATION MODE`)) +
    geom_bar() +
    labs(title = "Position Evaluation Mode",
         y = "POSITION COUNT")

  list("evaluation_mode_distribution" = evaluation_mode_distribution)
}

main <- function() {
  drive_analysis("Position Evaluation Mode",
                 analyze_database,
                 combine_analyses,
                 summarize_analyses,
                 export_as_tables,
                 import_as_tables,
                 visualize_analyses,
                 export_as_images)
}

main()
