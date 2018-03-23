#!/usr/bin/env Rscript

## TODO - no good way in R to find path of current script
##        perhaps the only good solution is to create a
##        package for the analysis ?
source("analysis/utils.R")
source("analysis/analysis.R")

ENVIRONMENT_METHODS <- c("environment",
                         "environment<-",
                         "parent.env",
                         "parent.env<-",
                         "globalenv",
                         "baseenv",
                         "emptyenv",
                         "new.env")

analyze_database <- function(database_filepath) {

    db <- src_sqlite(database_filepath)
    environments_tbl <- tbl(db, "environments")
    calls_tbl <- tbl(db, "calls")

    total_calls <-
      calls_tbl %>%
      summarize(`COUNT` = n()) %>%
      collect() %>%
      mutate(`FUNCTION NAME`="TOTAL CALLS")

    total_new_environments <-
      environments_tbl %>%
      summarize(`COUNT` = n()) %>%
      collect() %>%
      mutate(`FUNCTION NAME`="NewEnvironment")

    environment_function_usage <-
      calls_tbl %>%
      select(`FUNCTION NAME`=function_name) %>%
      filter(`FUNCTION NAME` %in% ENVIRONMENT_METHODS) %>%
      group_by(`FUNCTION NAME`) %>%
      summarize(`COUNT` = n()) %>%
      collect() %>%
      bind_rows(total_calls, total_new_environments) %>%
      mutate(`SCRIPT`=basename(file_path_sans_ext(database_filepath)))

  list("environment_function_usage" = environment_function_usage)
}

combine_analyses <- function(acc, element) {
  for(name in names(acc)) {
    acc[[name]] = bind_rows(acc[[name]], element[[name]])
  }
  acc
}

summarize_analyses <- function(analyses) {

  environment_function_usage <- analyses[["environment_function_usage"]]

  ## Filter scripts which fall in the lower quartile for each environment
  ## function call
  lower_quartile <-
    environment_function_usage %>%
    group_by(`FUNCTION NAME`) %>%
    do({
        filter(.data=., `COUNT` <= unname(quantile(`COUNT`, 0.25)))
    }) %>%
    spread(`FUNCTION NAME`, `COUNT`)

  ## Filter scripts which fall in the upper quartile for each environment
  ## function call
  upper_quartile <-
    environment_function_usage %>%
    group_by(`FUNCTION NAME`) %>%
    do({
        filter(.data=., `COUNT` >= unname(quantile(`COUNT`, 0.75)))
    }) %>%
    spread(`FUNCTION NAME`, `COUNT`)

  environment_function_usage_spread <-
    environment_function_usage %>%
    spread(`FUNCTION NAME`, `COUNT`) %>%
    replace(., is.na(.), 0)

  list("environment_function_usage" = environment_function_usage,
       "environment_function_usage_spread" = environment_function_usage_spread,
       "lower_quartile" = lower_quartile,
       "upper_quartile" = upper_quartile)
}

visualize_analyses <- function(analyses) {

  visualizations <- list()

  analyses$environment_function_usage %>%
    group_by(`FUNCTION NAME`) %>%
    do({
      name <- paste0(.$`FUNCTION NAME`[1])
      visualizations[[name]] <<-
        ggplot(data=., aes(`FUNCTION NAME`, `COUNT`)) +
        geom_boxplot() +
        geom_jitter()

      data.frame()
    })

  analyses$environment_function_usage %>%
    spread(`FUNCTION NAME`, `COUNT`) %>%
    gather(`FUNCTION NAME`, `COUNT`, -`TOTAL CALLS`, -`SCRIPT`) %>%
    mutate(`COUNT` = 100 * `COUNT` / `TOTAL CALLS`) %>%
    rename(`PROPORTION of CALLS (%)` = `COUNT`) %>%
    group_by(`FUNCTION NAME`) %>%
    do({
      name <- paste0(.$`FUNCTION NAME`[1], "-relative")
      visualizations[[name]] <<-
        ggplot(data=., aes(`FUNCTION NAME`, `PROPORTION of CALLS (%)`)) +
        geom_boxplot() +
        geom_jitter()

      data.frame()
    })

  visualizations
}

main <- function() {
  drive_analysis("Environment Manipulation Analysis",
                 analyze_database,
                 combine_analyses,
                 summarize_analyses,
                 export_as_tables,
                 import_as_tables,
                 visualize_analyses,
                 export_as_images)
}

main()
