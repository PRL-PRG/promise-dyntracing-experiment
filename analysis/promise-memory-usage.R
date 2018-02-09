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

  list("object_size_summary" = object_size_summary)

}

combine_analyses <- function(acc, element) {

  for(name in names(acc)) {
    acc[[name]] = bind_rows(acc[[name]], element[[name]])
  }
  acc
}

summarize_analyses <- function(analyses) {

  lower_count_quantile <-
    analyses$object_size_summary %>%
    group_by(`OBJECT TYPE`) %>%
    do({
      filter(.data=., `COUNT` <= unname(quantile(`COUNT`, 0.25)))
    }) %>%
    mutate_if(is.numeric, funs(round(., 3)))

  upper_count_quantile <-
    analyses$object_size_summary %>%
    select(-`SIZE`) %>%
    group_by(`OBJECT TYPE`) %>%
    do({
      filter(.data=., `COUNT` >= unname(quantile(`COUNT`, 0.75))) %>%
        select(-`OBJECT TYPE`)
    }) %>%
    mutate_if(is.numeric, funs(round(., 3)))

  lower_size_quantile <-
    analyses$object_size_summary %>%
    group_by(`OBJECT TYPE`) %>%
    do({
      filter(.data=., `SIZE` <= unname(quantile(`SIZE`, 0.25)))
    }) %>%
    mutate_if(is.numeric, funs(round(., 3)))

  upper_size_quantile <-
    analyses$object_size_summary %>%
    group_by(`OBJECT TYPE`) %>%
    do({
      filter(.data=., `SIZE` >= unname(quantile(`SIZE`, 0.75)))
    }) %>%
    mutate_if(is.numeric, funs(round(., 3)))

  append(analyses,
         list("lower_count_quantile" = lower_count_quantile,
              "upper_count_quantile" = upper_count_quantile,
              "lower_size_quantile" = lower_size_quantile,
              "upper_size_quantile" = upper_size_quantile))
}


visualize_analyses <- function(analyses) {
  promise_count_visualization <-
    analyses$object_size_summary %>%
    #filter(!`OBJECT TYPE` %in% c("CHAR", "CPLX", "INT", "LGL", "RAW", "REAL", "STR", "VEC")) %>%
    ggplot(aes(`OBJECT TYPE`, `COUNT` + 1)) +
    geom_boxplot() +
    geom_jitter(width = 0.2) +
    labs(title = "COUNT of R OBJECTS") +
    scale_y_continuous(trans = "log10") +
    #scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
    #               labels = trans_format("log10", math_format(10^.x))) +
    scale_fill_gdocs()

  promise_size_visualization <-
    analyses$object_size_summary %>%
    ggplot(aes(`OBJECT TYPE`, `SIZE`)) +
    geom_boxplot() +
    geom_jitter(width = 0.2) +
    labs(title = "SIZE of R OBJECTS",y = "SIZE (MB)") +
    scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                  labels = trans_format("log10", math_format(10^.x))) +
    scale_fill_gdocs()

  list("promise_count" = promise_count_visualization,
       "promise_size" = promise_size_visualization)

}


main <- function() {
  drive_analysis( "Promise Memory Usage Analysis",
                 analyze_database,
                 combine_analyses,
                 summarize_analyses,
                 export_as_tables,
                 import_as_tables,
                 visualize_analyses,
                 export_as_images)
}

main()
