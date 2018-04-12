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
    mutate(`OBJECT TYPE` = "PROMISE", `SIZE` = `COUNT` * 52)

  object_count_size <-
    type_distribution_table %>%
    group_by(type) %>%
    summarize(`COUNT` = n(), `SIZE` = n() * length) %>%
    collect() %>%
    rowwise() %>%
    mutate(`OBJECT TYPE` = typename(type)) %>%
    select(`OBJECT TYPE`, `COUNT`, `SIZE`) %>%
    rbind(promise_count)

  list("object_count_size" = object_count_size)

}

summarize_analyses <- function(analyses) {

  object_count_size <-
    analyses$object_count_size %>%
    group_by(`OBJECT TYPE`) %>%
    summarize(COUNT = sum(COUNT), SIZE = sum(SIZE)) %>%
    ungroup() %>%
    mutate(`RELATIVE SIZE` = 100 * `SIZE`/sum(`SIZE`),
           `RELATIVE COUNT` = 100 * `COUNT`/sum(`COUNT`),
           `GROUP TYPE` = ifelse(`OBJECT TYPE` == "PROMISE",
                                 "Promises", "Other Objects"))
  total_object_count <-
    analyses$object_count_size %>%
    pull(`COUNT`) %>%
    sum()

  total_object_size <-
    analyses$object_count_size %>%
    pull(`SIZE`) %>%
    sum()

  list("object_count_size" = object_count_size,
       "aggregate" = tibble(total_object_count = total_object_count,
                            total_object_size = total_object_size))

}

visualize_analyses <- function(analyses) {

  total_object_count <- analyses$aggregate$total_object_count

  object_count_visualization <-
    analyses$object_count_size %>%
    ggplot(aes(`GROUP TYPE`, weight = `COUNT`, fill=`OBJECT TYPE`)) +
    geom_bar() +
    scale_y_continuous(sec.axis = sec_axis(~ . * 100 / total_object_count,
                                           breaks = seq(0, 100, 10),
                                           labels = percent_labels),
                       labels = count_labels) +
    labs(y ="Count") +
    labs(x = NULL) +
    guides(title = NULL) +
    scale_fill_gdocs()

  total_object_size <- analyses$aggregate$total_object_size

  object_size_visualization <-
    analyses$object_count_size %>%
    rename("Object type" = `OBJECT TYPE`) %>%
    ggplot(aes(`GROUP TYPE`, weight = `RELATIVE SIZE`, fill=`Object type`)) +
    geom_bar() +
    scale_y_continuous(sec.axis = sec_axis(~ . * total_object_size / 100,
                                           labels = memory_size_labels),
                       labels = percent_labels) +
    labs(y ="Size (%)") +
    labs(x = NULL) +
    scale_fill_gdocs() +
    theme(legend.position = "bottom")

  list("object_count" = object_count_visualization,
       "object_size" = object_size_visualization)
}

latex_analyses <- function(analyses) {
  to_list(analyses$aggregate, 1)
}

latex_tables <- function(analyses) {
  list()
}

main <- function() {
  drive_analysis("Promise Memory Usage Analysis",
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
