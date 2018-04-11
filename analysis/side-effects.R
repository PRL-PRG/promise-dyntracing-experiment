#!/usr/bin/env Rscript

## TODO - no good way in R to find path of current script
##        perhaps the only good solution is to create a
##        package for the analysis ?
source("analysis/utils.R")
source("analysis/analysis.R")

purity_distribution <- function(environment_actions) {

  pure <-
    environment_actions %>%
    filter(direct_ena == 0, transitive_ena == 0,
           direct_end == 0, transitive_end == 0,
           direct_enr == 0, transitive_enr == 0)

  benign <-
    pure %>%
    filter(direct_enl == 0, transitive_enl == 0) %>%
    select(id)

  benign_count <-
    benign %>%
    tally() %>%
    collect()

  pure_count <-
    pure %>%
    tally() %>%
    collect()

  side_effecting_count <-
    environment_actions %>%
    filter(direct_ena > 0 | direct_end > 0 | direct_enr > 0) %>%
    tally() %>%
    collect()

  list(
    purity_distribution =
      tibble(
      "NATURE" = c("PURE", "SIDE EFFECTING"),
      "COUNT" = c(pure_count$n, side_effecting_count$n)
      ),
    benign = benign)
}

analyze_database <- function(database_filepath) {

  db <- src_sqlite(database_filepath)

  promises <-
    tbl(db, "promises")

  functions <-
    tbl(db, "functions") %>%
    collect()

  aggregated_environment_actions <-
    tbl(db, "aggregated_environment_actions") %>%
    collect()

  promise_environment_actions <-
    tbl(db, "promise_environment_actions") %>%
    filter(promise_id > 0) %>%
    rename(id = promise_id)

  call_environment_actions <-
    tbl(db, "function_environment_actions") %>%
    rename(id = function_id) %>%
    collect()

  function_environment_actions <-
    call_environment_actions %>%
    group_by(id) %>%
    summarize_all(sum)

  promise_distribution_output <- purity_distribution(promise_environment_actions)

  promise_purity_distribution <- promise_distribution_output$purity_distribution

  benign_promises <-
    promise_distribution_output$benign %>%
    left_join(promises, c("id" = "id")) %>%
    select(expression) %>%
    collect()

  call_distribution_output <- purity_distribution(call_environment_actions)

  call_purity_distribution <- call_distribution_output$purity_distribution

  benign_functions <-
    call_distribution_output$benign %>%
    left_join(functions, c("id" = "id")) %>%
    select(definition) %>%
    collect()

  list("promise_purity_distribution" = promise_purity_distribution,
       "call_purity_distribution" = call_purity_distribution,
       "function_environment_actions" = function_environment_actions,
       "aggregated_environment_actions" = aggregated_environment_actions,
       "benign_promises" = benign_promises,
       "benign_functions" = benign_functions)
}

combine_analyses <- function(acc, element) {
  for(name in names(acc)) {
    if(name == "function_environment_actions") {
      acc[[name]] <-
        acc[[name]] %>%
        bind_rows(element[[name]]) %>%
        group_by(id) %>%
        summarize_all(sum)
    }
    else {
      acc[[name]] = bind_rows(acc[[name]], element[[name]])
    }
  }
  acc
}

summarize_analyses <- function(analyses) {

  promise_purity_distribution <-
    analyses$promise_purity_distribution %>%
    group_by(`NATURE`) %>%
    summarize(`COUNT` = sum(`COUNT`))

  benign_promises <-
    analyses$benign_promises %>%
    group_by(expression) %>%
    summarize(count = n())

  call_purity_distribution <-
    analyses$call_purity_distribution %>%
    group_by(`NATURE`) %>%
    summarize(`COUNT` = sum(`COUNT`))

  benign_functions <-
    analyses$benign_functions %>%
    group_by(definition) %>%
    summarize(count = n())

  aggregated_environment_actions <-
    analyses$aggregated_environment_actions %>%
    group_by(context) %>%
    summarize_all(sum) %>%
    rename(`CONTEXT` = `context`)%>%
    gather(`ENVIRONMENT ACTION`, `COUNT`, -`CONTEXT`)

  list("promise_purity_distribution" = promise_purity_distribution,
       "call_purity_distribution" = call_purity_distribution,
       "aggregated_environment_actions" = aggregated_environment_actions,
       "benign_promises" = benign_promises,
       "benign_functions" = benign_functions)
}

visualize_analyses <- function(analyses) {

  promise_purity_distribution <-
    analyses$promise_purity_distribution %>%
    ggplot(aes(`NATURE`, weight=`COUNT`)) +
    geom_bar() +
    labs(title = "Promise count by purity", y = "COUNT")

  call_purity_distribution <-
    analyses$call_purity_distribution %>%
    ggplot(aes(`NATURE`, weight=`COUNT`)) +
    geom_bar() +
    labs(title = "Call count by purity", y = "COUNT")

  aggregated_environment_actions <-
    analyses$aggregated_environment_actions %>%
    ggplot(aes(`ENVIRONMENT ACTION`, weight=`COUNT`, fill=`CONTEXT`)) +
    geom_bar() +
    labs(title = "Environment Action distribution by context")

  list("promise_purity_distribution" = promise_purity_distribution,
       "call_purity_distribution" = call_purity_distribution,
       "aggregated_environment_actions" = aggregated_environment_actions)

}

latex_analyses <- function(analyses) {
  list()
}

main <- function() {
  drive_analysis("Side Effect Analysis",
                 analyze_database,
                 combine_analyses,
                 summarize_analyses,
                 export_as_tables,
                 import_as_tables,
                 visualize_analyses,
                 export_as_images,
                 latex_analyses,
                 export_as_latex_defs)
}

main()
