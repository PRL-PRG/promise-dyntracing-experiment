#!/usr/bin/env Rscript

## TODO - no good way in R to find path of current script
##        perhaps the only good solution is to create a
##        package for the analysis ?
source("analysis/utils.R")
source("analysis/analysis.R")

analyze_database <- function(database_filepath) {

  db <- src_sqlite(database_filepath)

  tbl_size <- collect(summarise(tbl(db, "functions"), n = n()))

  if(tbl_size$n == 0) return(NULL)

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
    select(id, type, definition)

  function_definitions <-
    tbl(db, "functions") %>%
    select(`FUNCTION ID` = id, DEFINITION = definition) %>%
    collect()

  evaluation_mode <-
    promise_associations_tbl %>%
    left_join(promise_evaluations_tbl, by = c("promise_id" = "promise_id")) %>%
    left_join(arguments_tbl, by = c("argument_id" = "id")) %>%
    left_join(calls_tbl, by = c("call_id" = "id")) %>%
    left_join(functions_tbl, by = c("function_id" = "id")) %>%
    select(-promise_id, -definition) %>%
    collect() %>%
    rename(`FUNCTION ID` = function_id, `ARGUMENT POSITION`=position) %>%
    group_by(`FUNCTION ID` , `ARGUMENT POSITION`) %>%
    summarize(`ALWAYS FORCED` = sum(!is.na(in_call_id)),
              `NEVER FORCED` = sum(is.na(in_call_id))) %>%
    ungroup() %>%
    mutate(`SCRIPT`=basename(file_path_sans_ext(database_filepath)))


  list("evaluation_mode" = evaluation_mode,
       "function_definitions" = function_definitions)
}

combine_analyses <- function(acc, element) {
  for(name in names(acc)) {
    acc[[name]] = bind_rows(acc[[name]], element[[name]])
  }
  acc
}

summarize_analyses <- function(analyses) {

  function_definitions <-
    analyses$function_definitions %>%
    group_by(`FUNCTION ID`) %>%
    summarize(`DEFINITION` = DEFINITION[[1]])

  evaluation_mode <-
    analyses$evaluation_mode %>%
    group_by(`FUNCTION ID` , `ARGUMENT POSITION`) %>%
    summarize(`ALWAYS FORCED` = sum(`ALWAYS FORCED`), `NEVER FORCED` = sum(`NEVER FORCED`)) %>%
    rowwise() %>%
    mutate(`EVALUATION MODE` = evaluation_modename(`ALWAYS FORCED`, `NEVER FORCED`))

  evaluation_mode_summarized_by_position <-
    evaluation_mode %>%
    group_by(`EVALUATION MODE`) %>%
    summarize(`COUNT` = n()) %>%
    add_column(`TYPE` = "POSITION")

  evaluation_mode_summarized_by_function <-
    evaluation_mode %>%
    group_by(`FUNCTION ID`) %>%
    summarize(`ALWAYS FORCED` = sum(`ALWAYS FORCED`), `NEVER FORCED` = sum(`NEVER FORCED`)) %>%
    rowwise() %>%
    mutate(`EVALUATION MODE` = evaluation_modename(`ALWAYS FORCED`, `NEVER FORCED`))

  never_forced_functions <-
    evaluation_mode_summarized_by_function %>%
    filter(`EVALUATION MODE` == "NEVER FORCED") %>%
    left_join(function_definitions, by=c("FUNCTION ID", "FUNCTION ID")) %>%
    select(`FUNCTION ID`, DEFINITION)

  evaluation_mode_summarized_by_function <-
    evaluation_mode_summarized_by_function %>%
    group_by(`EVALUATION MODE`) %>%
    summarize(`COUNT` = n()) %>%
    add_column(`TYPE` = "FUNCTION")

  evaluation_mode_summarized <-
    evaluation_mode_summarized_by_position %>%
    bind_rows(evaluation_mode_summarized_by_function)

  list("evaluation_mode" = evaluation_mode,
       "evaluation_mode_summarized" = evaluation_mode_summarized,
       "never_forced_functions" = never_forced_functions)
}


visualize_analyses <- function(analyses) {
  evaluation_mode_distribution <-
    analyses$evaluation_mode_summarized %>%
    ggplot(aes(`EVALUATION MODE`, weight=COUNT, fill=TYPE)) +
    geom_bar(position = "dodge") +
    labs(title = "Position Evaluation Mode",
         y = "COUNT")

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
