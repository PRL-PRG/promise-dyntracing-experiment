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
      select(argument_id = id, formal_parameter_position, name)

  promise_evaluations_tbl <-
    tbl(db, "promise_lifecycle") %>%
    select(promise_id, event_type) %>%
    filter(event_type == 1 || event_type == 3) %>%
    group_by(promise_id) %>%
    summarize(event_type = 1)

  promise_associations_tbl <-
    tbl(db, "promise_associations")

  calls_tbl <-
    tbl(db, "calls") %>%
    select(call_id = id, function_id, function_name, call_expression)

  functions_tbl <-
    tbl(db, "functions") %>%
    select(function_id = id, type, definition)

  function_definitions <-
    tbl(db, "functions") %>%
    select(`FUNCTION ID` = id, DEFINITION = definition) %>%
    collect()

  evaluation_mode_source_table <-
    promise_associations_tbl %>%
    left_join(promise_evaluations_tbl, by = c("promise_id" = "promise_id")) %>%
    #mutate(event_type = ifelse(is.na(promise_id), 1, event_type)) %>%
    left_join(arguments_tbl, by = c("argument_id" = "argument_id")) %>%
    left_join(calls_tbl, by = c("call_id" = "call_id")) %>%
    left_join(functions_tbl, by = c("function_id" = "function_id")) %>%
    select(-definition) %>%
    collect() %>%
    group_by(call_id) %>%
    mutate(weird = any(is.na(promise_id))) %>%
    ungroup() %>%
    filter(weird == FALSE)

  evaluation_mode <-
    evaluation_mode_source_table %>%
    rename(`FUNCTION ID` = function_id,
           `FORMAL PARAMETER POSITION`=formal_parameter_position) %>%
    group_by(`FUNCTION ID` , `FORMAL PARAMETER POSITION`) %>%
    summarize(`ALWAYS FORCED` = sum(!is.na(event_type)),
              `NEVER FORCED` = sum(is.na(event_type)),
              `FUNCTION NAME`= function_name[[1]]) %>%
    ungroup() %>%
    mutate(`SCRIPT`=basename(file_path_sans_ext(database_filepath)))

  sometimes_never_functions <-
    evaluation_mode %>%
    group_by(`FUNCTION ID`) %>%
    summarize(`NOT STRICT` = sum(`NEVER FORCED`)) %>%
    filter(`NOT STRICT` != 0)

  sometimes_never_calls <-
    evaluation_mode_source_table %>%
    filter(`function_id` %in% sometimes_never_functions[["FUNCTION ID"]]) %>%
    select(function_id, function_name, call_id, call_expression, formal_parameter_position, name, promise_id, event_type,)

  list("evaluation_mode" = evaluation_mode,
       "function_definitions" = function_definitions,
       "sometimes_never_calls" = sometimes_never_calls)
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
    group_by(`FUNCTION ID` , `FORMAL PARAMETER POSITION`, `FUNCTION NAME`) %>%
    summarize(`ALWAYS FORCED` = sum(`ALWAYS FORCED`), `NEVER FORCED` = sum(`NEVER FORCED`)) %>%
    rowwise() %>%
    mutate(`EVALUATION MODE` = evaluation_modename(`ALWAYS FORCED`, `NEVER FORCED`))

  evaluation_mode_summarized_by_position <-
    evaluation_mode %>%
    group_by(`EVALUATION MODE`) %>%
    summarize(`COUNT` = n()) %>%
    add_column(`TYPE` = "FORMAL PARAMETER POSITION")

  evaluation_mode_summarized_by_function <-
    evaluation_mode %>%
    group_by(`FUNCTION ID`) %>%
    summarize(`EVALUATION MODE` = function_evaluation_modename(`EVALUATION MODE`),
              `FUNCTION NAME`=`FUNCTION NAME`[[1]])

  partial_functions <-
    evaluation_mode_summarized_by_function %>%
    filter(`EVALUATION MODE` == "PARTIAL") %>%
    left_join(function_definitions, by=c("FUNCTION ID", "FUNCTION ID")) %>%
    select(`FUNCTION ID`, DEFINITION, `FUNCTION NAME`)

  partial_functions_no_def <-
    partial_functions %>%
    select(`FUNCTION ID`, `FUNCTION NAME`) %>%
    mutate(`PACKAGE NAME` = extract_package_name(`FUNCTION NAME`))

  partial_by_package <-
    partial_functions_no_def %>%
    group_by(`PACKAGE NAME`) %>%
    summarize(`COUNT` = n())

  sometimes_functions <-
    evaluation_mode_summarized_by_function %>%
    filter(`EVALUATION MODE` == "SOMETIMES") %>%
    left_join(function_definitions, by=c("FUNCTION ID", "FUNCTION ID")) %>%
    select(`FUNCTION ID`, DEFINITION, `FUNCTION NAME`)

  sometimes_functions_no_def <-
    sometimes_functions %>%
    select(`FUNCTION ID`, `FUNCTION NAME`) %>%
    mutate(`PACKAGE NAME` = extract_package_name(`FUNCTION NAME`))

  sometimes_by_package <-
    sometimes_functions_no_def %>%
    group_by(`PACKAGE NAME`) %>%
    summarize(`COUNT` = n())

  evaluation_mode_summarized_by_function <-
    evaluation_mode_summarized_by_function %>%
    group_by(`EVALUATION MODE`) %>%
    summarize(`COUNT` = n()) %>%
    add_column(`TYPE` = "FUNCTION")

  evaluation_mode_summarized <-
    evaluation_mode_summarized_by_position %>%
    bind_rows(evaluation_mode_summarized_by_function)

  sometimes_never_calls <-
    analyses$sometimes_never_calls %>%
    arrange(function_name, call_id)

  list("evaluation_mode" = evaluation_mode,
       "evaluation_mode_summarized" = evaluation_mode_summarized,
       "partial_functions" = partial_functions,
       "partial_functions_no_def" = partial_functions_no_def,
       "partial_by_package" = partial_by_package,
       "sometimes_functions" = sometimes_functions,
       "sometimes_functions_no_def" = sometimes_functions_no_def,
       "sometimes_by_package" = sometimes_by_package,
       "sometimes_never_calls" = sometimes_never_calls)
}

visualize_analyses <- function(analyses) {
  evaluation_mode_distribution_by_function <-
    analyses$evaluation_mode_summarized %>%
    filter(`TYPE` == "FUNCTION") %>%
    ggplot(aes(`EVALUATION MODE`, weight=COUNT)) + #, fill=TYPE)) +
    geom_bar(position = "dodge") +
    labs(title = "Function Evaluation Mode",
         y = "COUNT")

    evaluation_mode_distribution_by_formal_parameter_position <-
    analyses$evaluation_mode_summarized %>%
    filter(`TYPE` == "FORMAL PARAMETER POSITION") %>%
    ggplot(aes(`EVALUATION MODE`, weight=COUNT)) + #, fill=TYPE)) +
    geom_bar(position = "dodge") +
    labs(title = "Position Evaluation Mode",
         y = "COUNT")

  list("evaluation_mode_distribution_by_function" =
         evaluation_mode_distribution_by_function,
       "evaluation_mode_distribution_by_formal_parameter_position" =
         evaluation_mode_distribution_by_formal_parameter_position)
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
