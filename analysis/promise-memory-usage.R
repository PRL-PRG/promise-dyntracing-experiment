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

  raw <-
    type_distribution_table %>%
    group_by(type) %>%
    summarize(`COUNT` = n(), `SIZE` = n() * length) %>%
    collect() %>%
    rowwise() %>%
    mutate(`OBJECT TYPE` = typename(type)) %>%
    select(-type) %>%
    rbind(promise_count) %>%
    mutate(`SCRIPT` = script) %>%
    group_by(`SCRIPT`) %>%
    mutate(`RELATIVE SIZE` = `SIZE` / sum(`SIZE`),
           `RELATIVE COUNT` = `COUNT` / sum(`COUNT`)) %>%
    ungroup() %>%
    mutate_if(is.numeric, funs(round(., 3))) %>%
    select(`SCRIPT`, `OBJECT TYPE`, `COUNT`, `RELATIVE COUNT`, `SIZE`, `RELATIVE SIZE`)

  list("raw" = raw)

}

combine_analyses <- function(acc, element) {

  for(name in names(acc)) {
    acc[[name]] = bind_rows(acc[[name]], element[[name]])
  }
  acc
}

summarize_analyses <- function(analyses) {
  summary <-
    analyses$raw %>%
    group_by(`OBJECT TYPE`) %>%
    summarize(COUNT = sum(COUNT), SIZE = sum(SIZE)) %>%
    mutate(`RELATIVE SIZE` = round(SIZE / sum(SIZE), 2),
           `RELATIVE COUNT` = round(COUNT / sum(COUNT), 2))

  append(analyses,
         list("summary" = summary))
}


visualize_analyses <- function(analyses) {

  object_count_visualization <-
    analyses$summary %>%
    ggplot(aes(`OBJECT TYPE`, weight = `COUNT`, fill=`OBJECT TYPE`)) +
    geom_bar() +
    scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                  labels = count_labels) + #trans_format("log10", math_format(10^.x))) +
    labs(title = "Distribution of object count by type",
         y ="TOTAL COUNT (log10 scale)") +
    scale_fill_gdocs() +
    guides(fill=FALSE)

  relative_object_count_visualization <-
    analyses$summary %>%
    ggplot(aes(`OBJECT TYPE`, weight=`RELATIVE COUNT`, fill=`OBJECT TYPE`)) +
    geom_bar() +
    scale_y_continuous(labels=relative_labels) +
    labs(title = "Distribution of relative object count by type",
         y = "RELATIVE COUNT") +
    scale_fill_gdocs() +
    guides(fill=FALSE)

  object_size_visualization <-
    analyses$summary %>%
    ggplot(aes(`OBJECT TYPE`, weight=`SIZE`, fill = `OBJECT TYPE`)) +
    geom_bar() +
    labs(title = "Distribution of object size by type", y = "SIZE (log2 scale)") +
    scale_y_continuous(trans="log2",
                       breaks = trans_breaks("log2", function(x) 2^x),
                       labels = memory_size_labels) + #trans_format("log2", math_format(2^.x))) +
    scale_fill_gdocs() +
    guides(fill=FALSE)

  relative_object_size_visualization <-
    analyses$summary %>%
    ggplot(aes(`OBJECT TYPE`, weight=`RELATIVE SIZE`, fill = `OBJECT TYPE`)) +
    geom_bar() +
    scale_y_continuous(labels=relative_labels) +
    labs(title = "Distribution of relative object size by type",
         y = "RELATIVE SIZE") +
    scale_fill_gdocs() +
    guides(fill=FALSE)

  list("object_count" = object_count_visualization,
       "relative_object_count" = relative_object_count_visualization,
       "object_size" = object_size_visualization,
       "relative_object_size" = relative_object_size_visualization)
}


main <- function() {
  drive_analysis("Promise Memory Usage Analysis",
                 analyze_database,
                 combine_analyses,
                 summarize_analyses,
                 export_as_tables,
                 import_as_tables,
                 visualize_analyses,
                 export_as_images)
}

main()
