#!/usr/bin/env Rscript

## TODO - no good way in R to find path of current script
##        perhaps the only good solution is to create a
##        package for the analysis ?
source("analysis/utils.R")
source("analysis/analysis.R")

analyze_database <- function(database_filepath) {

  script <- basename(file_path_sans_ext(database_filepath))

  db <- src_sqlite(database_filepath)

  type_distribution_table <-
    db %>%
    tbl("type_distribution")

  lifecycle_table <-
    db %>%
    tbl("promise_lifecycle")

  promise_count <-
    lifecycle_table %>%
    filter(event_type == 0) %>%
    summarize(`COUNT` = n()) %>%
    collect() %>%
    mutate(`OBJECT TYPE` = "PROMISE", `SIZE` = `COUNT` * 52 / (1024 * 1024))
    
  object_size_summary <-
    type_distribution_table %>%
    group_by(type) %>%
    summarize(`COUNT` = n(), `SIZE` = n() * length / (1024 * 1024)) %>%
    collect() %>%
    rowwise() %>%
    mutate(`OBJECT TYPE` = typename(type)) %>%
    select(-type) %>%
    rbind(promise_count) %>%
    mutate(`SCRIPT` = script)

  lower_count_quantile <-
    object_size_summary %>%
    group_by(`OBJECT TYPE`) %>%
    do({
        filter(.data=., `COUNT` <= unname(quantile(`COUNT`, 0.25)))
    })

  upper_count_quantile <-
    object_size_summary %>%
    group_by(`OBJECT TYPE`) %>%
    do({
        filter(.data=., `COUNT` >= unname(quantile(`COUNT`, 0.75)))
    })

  lower_size_quantile <-
    object_size_summary %>%
    group_by(`OBJECT TYPE`) %>%
    do({
        filter(.data=., `SIZE` <= unname(quantile(`SIZE`, 0.25)))
    })

  upper_size_quantile <-
    object_size_summary %>%
    group_by(`OBJECT TYPE`) %>%
    do({
        filter(.data=., `SIZE` >= unname(quantile(`SIZE`, 0.75)))
    })

  list("object_size_summary" = object_size_summary,
       "lower_count_quantile" = lower_count_quantile,
       "upper_count_quantile" = upper_count_quantile,
       "lower_size_quantile" = lower_size_quantile,
       "upper_size_quantile" = upper_size_quantile)

}

export_analyses <- function(analyses, table_dir) {
  for(name in names(analyses)) {
    analyses[[name]] %>%
      write_csv(file.path(table_dir, paste0(name, ".csv")))
  }
}

visualize_analyses <- function(analyses) {
  promise_count_visualization <-
    analyses$object_size_summary %>%
    ggplot(aes(`OBJECT TYPE`, `COUNT`)) +
    geom_boxplot() +
    geom_jitter(width = 0.2) +
    labs(title = "COUNT of R OBJECTS") +
    scale_fill_gdocs()

  promise_size_visualization <-
    analyses$object_size_summary %>%
    ggplot(aes(`OBJECT TYPE`, `SIZE`)) +
    geom_boxplot() +
    geom_jitter(width = 0.2) +
    labs(title = "SIZE of R OBJECTS",y = "SIZE (MB)") +
    scale_fill_gdocs()

  list("promise_count" = promise_count_visualization,
       "promise_size" = promise_size_visualization)

}

combine_analyses <- function(acc, element) {
  for(name in names(acc)) {
    acc[[name]] = bind_rows(acc[[name]], element[[name]])
  }
  acc
}

export_visualizations <- function(visualizations, graph_dir) {
  ggsave(plot=visualizations$promise_count,
         filename=file.path(graph_dir, "promise-count.png"))
  ggsave(plot=visualizations$promise_size,
         filename=file.path(graph_dir, "promise-size.png"))
}

main <- function() {
  drive_analysis(analyze_database,
                 combine_analyses,
                 export_analyses,
                 visualize_analyses,
                 export_visualizations)
}

main()
