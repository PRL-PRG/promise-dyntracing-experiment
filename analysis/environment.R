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

  list("environment-function-usage" = environment_function_usage)
}

combine_analyses <- function(acc, element) {
  for(name in names(acc)) {
    acc[[name]] = bind_rows(acc[[name]], element[[name]])
  }
  acc
}

summarize_analyses <- function(analyses) {

  ## Filter scripts which fall in the lower quartile for each environment
  ## function call
  environment_function_usage <- analyses[["environment-function-usage"]]

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

  list("environment-function-usage" = environment_function_usage,
       "environment-function-usage-spread" = environment_function_usage_spread,
       "lower-quartile" = lower_quartile,
       "upper-quartile" = upper_quartile)
}

export_analyses <- function(analyses, table_dir) {
  analyses %>%
    iwalk(
      function(table, tablename)
        table %>%
        write_csv(file.path(table_dir, paste0(tablename,".csv"))))
}

visualize_analyses <- function(analyses) {

  visualizations <- list()

  analyses$`environment-function-usage` %>%
    group_by(`FUNCTION NAME`) %>%
    do({
      name <- paste0(.$`FUNCTION NAME`[1])
      visualizations[[name]] <<-
        ggplot(data=., aes(`FUNCTION NAME`, `COUNT` + 1)) +
        geom_violin() +
        geom_boxplot(width = 0.2) +
        #geom_boxplot() +
        scale_y_continuous(trans = "log10")

      data.frame()
    })

  analyses$`environment-function-usage` %>%
    spread(`FUNCTION NAME`, `COUNT`) %>%
    gather(`FUNCTION NAME`, `COUNT`, -`TOTAL CALLS`, -`SCRIPT`) %>%
    mutate(`COUNT` = 100 * `COUNT` / `TOTAL CALLS`) %>%
    rename(`PROPORTION of CALLS (%)` = `COUNT`) %>%
    group_by(`FUNCTION NAME`) %>%
    do({
      name <- paste0(.$`FUNCTION NAME`[1], "-relative")
      visualizations[[name]] <<-
        ggplot(data=., aes(`FUNCTION NAME`, `PROPORTION of CALLS (%)`)) +
        geom_violin(draw_quantiles = c(0.25, 0.5, 0.75))
      data.frame()
    })

  visualizations
}

export_visualizations <- function(visualizations, graph_dir) {
  visualizations %>%
    iwalk(
      function(graph, graphname)
        ggsave(plot = graph,
               filename = file.path(graph_dir,
                                    paste0(graphname,".png"))))
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


## main <- function() {
##   arguments <- parse_program_arguments()
##   input_dir = arguments$args[1]
##   table_dir = arguments$args[2]
##   graph_dir = arguments$args[3]
##   datatable <- create_table(input_dir)
##   export_table(datatable, file.path(table_dir, "environment-usage.tsv"))
##   analyze_table(datatable,
##                 file.path(table_dir, "environment-usage-lower-quartile.tsv"),
##                 file.path(table_dir, "environment-usage-upper-quartile.tsv"))
##   create_graph(datatable, file.path(graph_dir, "fun-"))
## }

## create_graph <- function(datatable, output_file) {
##   #https://stackoverflow.com/questions/29034863/apply-a-ggplot-function-per-group-with-dplyr-and-set-title-per-group
##   datatable %>%
##     group_by(`FUNCTION NAME`) %>%
##     do({
##       plot <-
##         ggplot(data=., aes(`FUNCTION NAME`, `COUNT`)) +
##         geom_violin() +
##         geom_boxplot(width = 0.2)
##       filename <- paste0(output_file,.$`FUNCTION NAME`[1],".png")
##       ggsave(plot=plot, filename=filename)
##       data.frame(filename=filename)
##     })


##   datatable %>%
##     spread(`FUNCTION NAME`, `COUNT`) %>%
##     gather(`FUNCTION NAME`, `COUNT`, -`TOTAL CALLS`, -`RUNNABLE`) %>%
##     mutate(`COUNT` = 100 * `COUNT` / `TOTAL CALLS`) %>%
##     group_by(`FUNCTION NAME`) %>%
##     do({
##       p <-
##         ggplot(data=., aes(`FUNCTION NAME`, `COUNT`)) +
##         geom_violin(draw_quantiles = c(0.25, 0.5, 0.75))
##       filename <- paste0(output_file,.$`FUNCTION NAME`[1],"-relative.png")
##       ggsave(plot=p, filename=filename)
##       data.frame(filename=filename)
##     })
## }
