#!/usr/bin/env Rscript

## TODO - no good way in R to find path of current script
##        perhaps the only good solution is to create a
##        package for the analysis ?
source("analysis/utils.R")
source("analysis/analysis.R")

analyze_database <- function(database_filepath) {

  db <- src_sqlite(database_filepath)

  all_promises <-
    tbl(db, "promise_evaluations") %>%
    filter(promise_id >= 0) %>%
    select(promise_id) %>%
    collect()


  variable_actions <-
    tbl(db, "variable_actions") %>%
    filter(promise_id >= 0) %>%
    select(promise_id, action) %>%
    collect()


  extra_promises <- length(setdiff(unique(all_promises[["promise_id"]]), variable_actions[["promise_id"]]))

  if(nrow(variable_actions) == 0) return(NULL)

  promise_associations <-
    tbl(db, "promise_associations") %>%
    filter(promise_id >= 0) %>%
    select(promise_id, call_id)

  calls <-
    tbl(db, "calls") %>%
    select(call_id = id, function_id)

  functions <-
    tbl(db, "functions") %>%
    select(function_id = id, definition)

  promise_id_to_function_id <-
    promise_associations %>%
    left_join(calls, by = c("call_id" = "call_id")) %>%
    left_join(functions, by = c("function_id" = "function_id")) %>%
    group_by(promise_id) %>%
    summarize(function_id = function_id, definition = definition) %>%
    collect()

  variable_actions <-
    variable_actions %>%
    group_by(promise_id, action) %>%
    summarize(count = n()) %>%
    left_join(promise_id_to_function_id, by = c("promise_id" = "promise_id")) %>%
    print()

  action_count_distribution <-
    variable_actions %>%
    group_by(action) %>%
    summarize(action_count = sum(count)) %>%
    rename(`ACTION` = action, `ACTION COUNT` = action_count) %>%
    add_column(SCRIPT = basename(file_path_sans_ext(database_filepath)), .before = 1)

  action_promise_count_distribution <-
    variable_actions %>%
    group_by(action) %>%
    summarize(promise_count = n_distinct(promise_id)) %>%
    add_row(action = "none", promise_count = extra_promises) %>%
    rename(`ACTION` = action, `PROMISE COUNT` = promise_count) %>%
    add_column(SCRIPT = basename(file_path_sans_ext(database_filepath)), .before = 1)

  purity_distribution <-
    variable_actions %>%
    spread(action, count) %>%
    mutate(asn = if (exists('asn', where = .)) asn else 0) %>%
    mutate(def = if (exists('def', where = .)) def else 0) %>%
    mutate(rem = if (exists('rem', where = .)) rem else 0) %>%
    replace(., is.na(.), 0) %>%
    group_by(promise_id) %>%
    summarize(PURITY = (asn + def + rem) == 0, function_id = function_id, definition = definition)

  function_purity_distribution <-
    purity_distribution %>%
    print() %>%
    group_by(function_id) %>%
    summarize(definition = first(definition), PURITY = all(PURITY))

  promise_purity_distribution <-
    purity_distribution %>%
    group_by(`PURITY`) %>%
    summarize(`PROMISE COUNT` = n()) %>%
    rowwise() %>%
    mutate(`PURITY` = purityname(`PURITY`)) %>%
    add_row(`PURITY` = "BENIGN", `PROMISE COUNT` = extra_promises) %>%
    add_column(SCRIPT = basename(file_path_sans_ext(database_filepath)), .before = 1)

  list("action_count_distribution" = action_count_distribution,
       "action_promise_count_distribution" = action_promise_count_distribution,
       "promise_purity_distribution" = promise_purity_distribution,
       "function_purity_distribution" = function_purity_distribution)
}

combine_analyses <- function(acc, element) {
  for(name in names(acc)) {
    acc[[name]] = bind_rows(acc[[name]], element[[name]])
  }
  acc
}

summarize_analyses <- function(analyses) {

  action_count_distribution_total <-
    analyses$action_count_distribution %>%
    group_by(ACTION) %>%
    summarize(`ACTION COUNT` = sum(`ACTION COUNT`))

  action_promise_count_distribution_total <-
    analyses$action_promise_count_distribution %>%
    group_by(ACTION) %>%
    summarize(`PROMISE COUNT` = sum(`PROMISE COUNT`))

  promise_purity_distribution_total <-
    analyses$promise_purity_distribution %>%
    group_by(`PURITY`) %>%
    summarize(`PROMISE COUNT` = sum(`PROMISE COUNT`))

  function_purity_distribution_total <-
    analyses$function_purity_distribution %>%
    print() %>%
    group_by(function_id) %>%
    summarize(definition = first(definition), PURITY = purityname(all(PURITY))) %>%
    group_by(`PURITY`) %>%
    summarize(`FUNCTION COUNT` = n_distinct(function_id))

  append(analyses,
         list("action_count_distribution_total" = action_count_distribution_total,
              "action_promise_count_distribution_total" = action_promise_count_distribution_total,
              "promise_purity_distribution_total" = promise_purity_distribution_total,
              "function_purity_distribution_total" = function_purity_distribution_total))
}

visualize_analyses <- function(analyses) {
  action_count_distribution <-
    analyses$action_count_distribution_total %>%
    ggplot(aes(`ACTION`, weight=`ACTION COUNT`)) +
    geom_bar() +
    labs(title = "Action count", y = "ACTION COUNT")

  action_promise_count_distribution <-
    analyses$action_promise_count_distribution_total %>%
    ggplot(aes(`ACTION`, weight=`PROMISE COUNT`)) +
    geom_bar() +
    labs(title = "Promise count by action", y = "PROMISE COUNT")

  promise_purity_distribution <-
    analyses$promise_purity_distribution_total %>%
    ggplot(aes(`PURITY`, weight=`PROMISE COUNT`)) +
    geom_bar() +
    labs(title = "Promise count by purity", y = "PROMISE COUNT")

  function_purity_distribution <-
    analyses$function_purity_distribution_total %>%
    ggplot(aes(`PURITY`, weight=`FUNCTION COUNT`)) +
    geom_bar() +
    labs(title = "Function count by purity", y = "FUNCTION COUNT")

  list("action_count_distribution" = action_count_distribution,
       "action_promise_count_distribution" = action_promise_count_distribution,
       "promise_purity_distribution" = promise_purity_distribution,
       "function_purity_distribution" = function_purity_distribution)
}

main <- function() {
  drive_analysis("Side Effect Analysis",
                 analyze_database,
                 combine_analyses,
                 summarize_analyses,
                 export_as_tables,
                 import_as_tables,
                 visualize_analyses,
                 export_as_images)
}

main()
