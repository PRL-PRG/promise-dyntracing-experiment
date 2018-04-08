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
      select(argument_id = id, formal_parameter_position, name, call_id)

  promise_evaluations_tbl <-
    tbl(db, "promise_evaluations") %>%
    group_by(promise_id) %>%
    summarize(count = n())

  promise_associations_tbl <-
    tbl(db, "promise_associations") %>%
    select(promise_id, argument_id)

  calls_tbl <-
    tbl(db, "calls") %>%
    select(call_id = id, function_id)

  functions_tbl <-
    tbl(db, "functions") %>%
    select(id, type, definition)

  function_definitions <-
    tbl(db, "functions") %>%
    select(`FUNCTION ID` = id, DEFINITION = definition) %>%
    collect()

  evaluation_mode <-
    calls_tbl %>%
    left_join(arguments_tbl, by = c("call_id" = "call_id")) %>%
    left_join(promise_associations_tbl, by = c("argument_id" = "argument_id")) %>%
    left_join(promise_evaluations_tbl, by = c("promise_id" = "promise_id")) %>%
    filter(!is.na(formal_parameter_position)) %>%
    group_by(function_id, call_id, formal_parameter_position) %>%
    summarize(`POSITION ALWAYS FORCED` = !is.na(count),
              `POSITION NEVER FORCED` = is.na(count)) %>%
    summarize(`CALL ALWAYS FORCED` = sum(`POSITION ALWAYS FORCED`),
              `CALL NEVER FORCED` = sum(`POSITION NEVER FORCED`)) %>%
    summarize(`ALWAYS FORCED` = sum(`CALL ALWAYS FORCED`),
              `NEVER FORCED` = sum(`CALL NEVER FORCED`)) %>%
    rename(`FUNCTION ID` = function_id) %>%
    collect()

##   print("here")
##   print(nrow(collect(r1)))
##   print(nrow(collect(r2)))

##   evaluation_mode <-
##     promise_associations_tbl %>%
##     left_join(promise_evaluations_tbl, by = c("promise_id" = "promise_id")) %>%
##     left_join(arguments_tbl, by = c("argument_id" = "id")) %>%
##     left_join(calls_tbl, by = c("call_id" = "id")) %>%
##     left_join(functions_tbl, by = c("function_id" = "id")) %>%
##     select(-promise_id, -definition) %>%
## ##    select(name) %>%
##     collect() %>%
##     ## rowwise() %>%
##     ## mutate(new_name = update_argument_name(name)) %>%
##     ## group_by(new_name) %>%
##     ## summarize(count = n()) %>%
##     ## print() %>%
##     group_by(function_id) %>%
##     summarize(`ALWAYS FORCED` = sum(!is.na(event_type)),
##               `NEVER FORCED` = sum(is.na(event_type))) %>%
##     rename(`FUNCTION ID` = function_id) %>%
##     ungroup() %>%
##     mutate(`SCRIPT`=basename(file_path_sans_ext(database_filepath)))

  list("evaluation_mode" = evaluation_mode,
       "function_definitions" = function_definitions)
}

summarize_analyses <- function(analyses) {

  function_definitions <-
    analyses$function_definitions %>%
    group_by(`FUNCTION ID`) %>%
    summarize(`DEFINITION` = DEFINITION[[1]])

  evaluation_mode <-
    analyses$evaluation_mode %>%
    group_by(`FUNCTION ID`) %>%
    summarize(`EVALUATION MODE` = evaluation_modename(sum(`ALWAYS FORCED`), sum(`NEVER FORCED`)))


  ## evaluation_mode_summarized_by_position <-
  ##   evaluation_mode %>%
  ##   group_by(`EVALUATION MODE`) %>%
  ##   summarize(`COUNT` = n()) %>%
  ##   add_column(`TYPE` = "POSITION")

  ## evaluation_mode_summarized_by_function <-
  ##   evaluation_mode %>%
  ##   group_by(`FUNCTION ID`) %>%
  ##   summarize(`ALWAYS FORCED` = sum(`ALWAYS FORCED`), `NEVER FORCED` = sum(`NEVER FORCED`)) %>%
  ##   rowwise() %>%
  ##   mutate(`EVALUATION MODE` = evaluation_modename(`ALWAYS FORCED`, `NEVER FORCED`))

  never_forced_functions <-
    evaluation_mode %>%
    filter(`EVALUATION MODE` == "NEVER FORCED") %>%
    left_join(function_definitions, by=c("FUNCTION ID", "FUNCTION ID")) %>%
    select(`FUNCTION ID`, DEFINITION)

  sometimes_forced_functions <-
    evaluation_mode %>%
    filter(`EVALUATION MODE` == "SOMETIMES FORCED") %>%
    left_join(function_definitions, by=c("FUNCTION ID", "FUNCTION ID")) %>%
    select(`FUNCTION ID`, DEFINITION)

  evaluation_mode_summarized_by_function <-
    evaluation_mode %>%
    group_by(`EVALUATION MODE`) %>%
    summarize(`COUNT` = n())

  list("evaluation_mode" = evaluation_mode,
       "evaluation_mode_summarized_by_function" = evaluation_mode_summarized_by_function,
       "never_forced_functions" = never_forced_functions,
       "sometimes_forced_functions" = sometimes_forced_functions)
}


visualize_analyses <- function(analyses) {
  evaluation_mode_distribution <-
    analyses$evaluation_mode_summarized_by_function %>%
    ggplot(aes(`EVALUATION MODE`, weight=COUNT)) +
    geom_bar(position = "dodge") +
    labs(title = "Function Evaluation Mode",
         y = "COUNT")

  list("evaluation_mode_distribution" = evaluation_mode_distribution)
}

main <- function() {
  drive_analysis("Function force",
                 analyze_database,
                 combine_analyses,
                 summarize_analyses,
                 export_as_tables,
                 import_as_tables,
                 visualize_analyses,
                 export_as_images)
}

main()
