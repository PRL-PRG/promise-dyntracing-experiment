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
    mutate(`SCRIPT` = script) %>%
    group_by(`SCRIPT`) %>%
    mutate(`RELATIVE SIZE` = `SIZE` / sum(`SIZE`),
           `RELATIVE COUNT` = `COUNT` / sum(`COUNT`)) %>%
    ungroup() %>%
    mutate_if(is.numeric, funs(round(., 3))) %>%
    select(`SCRIPT`, `OBJECT TYPE`, `COUNT`, `RELATIVE COUNT`, `SIZE`, `RELATIVE SIZE`)

  list("object_size_summary" = object_size_summary)

}

combine_analyses <- function(acc, element) {

  for(name in names(acc)) {
    acc[[name]] = bind_rows(acc[[name]], element[[name]])
  }
  acc
}

summarize_analyses <- function(analyses) {

  quantiles <-
    analyses$object_size_summary %>%
    nest(-`OBJECT TYPE`) %>%
    mutate(Quantiles = map(data, ~ quantile(.$SIZE, na.rm = TRUE))) %>%
    unnest(map(Quantiles, tidy)) %>%
    add_column(QUANTITY = "SIZE")

  quantiles <-
    analyses$object_size_summary %>%
    nest(-`OBJECT TYPE`) %>%
    mutate(Quantiles = map(data, ~ quantile(.$`RELATIVE SIZE`, na.rm = TRUE))) %>%
    unnest(map(Quantiles, tidy)) %>%
    add_column(QUANTITY = "RELATIVE SIZE") %>%
    rbind(quantiles)

  quantiles <-
    analyses$object_size_summary %>%
    nest(-`OBJECT TYPE`) %>%
    mutate(Quantiles = map(data, ~ quantile(.$COUNT, na.rm = TRUE))) %>%
    unnest(map(Quantiles, tidy)) %>%
    add_column(`QUANTITY` = "COUNT") %>%
    rbind(quantiles)

  quantiles <-
    analyses$object_size_summary %>%
    nest(-`OBJECT TYPE`) %>%
    mutate(Quantiles = map(data, ~ quantile(.$`RELATIVE COUNT`, na.rm = TRUE))) %>%
    unnest(map(Quantiles, tidy)) %>%
    add_column(`QUANTITY` = "RELATIVE COUNT") %>%
    rbind(quantiles) %>%
    rename(QUANTILE = names, VALUE = x)

  outliers <-
    analyses$object_size_summary %>%
    group_by(`OBJECT TYPE`) %>%
    do({
      a <-
        filter(.data=., `COUNT` >= unname(quantile(`COUNT`, 0.75, na.rm = TRUE))) %>%
        add_column(QUANTITY = "COUNT")

      b <-
        filter(.data=., `RELATIVE COUNT` >= unname(quantile(`RELATIVE COUNT`, 0.75, na.rm = TRUE))) %>%
        add_column(QUANTITY = "RELATIVE COUNT")

      c <-
        filter(.data=., SIZE >= unname(quantile(SIZE, 0.75, na.rm = TRUE))) %>%
        add_column(QUANTITY = "SIZE")

      d <-
        filter(.data=., `RELATIVE SIZE` >= unname(quantile(`RELATIVE SIZE`, 0.75, na.rm = TRUE))) %>%
        add_column(QUANTITY = "RELATIVE SIZE")

      bind_rows(a, b, c, d)
    }) %>%
    select(`OBJECT TYPE`, `QUANTITY`, `COUNT`, `RELATIVE COUNT`, `SIZE`, `RELATIVE SIZE`, `SCRIPT`)

  append(analyses,
         list("quantiles" = quantiles,
              "outliers" = outliers))
}


visualize_analyses <- function(analyses) {

  promise_count_visualization <-
    analyses$object_size_summary %>%
    ggplot(aes(`OBJECT TYPE`, `COUNT` + 1, fill=`OBJECT TYPE`)) +
    geom_violin(draw_quantiles = c(0.25, 0.5, 0.75), scale = "width") +
    scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                  labels = trans_format("log10", math_format(10^.x))) +
    labs(title = "Distribution of object count by type",
         y ="COUNT + 1 (log10 scale)") +
    scale_fill_gdocs() +
    guides(fill=FALSE)

  relative_count_visualization <-
    analyses$object_size_summary %>%
    ggplot(aes(`OBJECT TYPE`, `RELATIVE COUNT`, fill=`OBJECT TYPE`)) +
    geom_violin(draw_quantiles = c(0.25, 0.5, 0.75), scale = "width") +
    labs(title = "Distribution of relative object count by type") +
    scale_fill_gdocs() +
    guides(fill=FALSE)

  ##geom_jitter(width = 0.2) + ##scale = "width", width = 1.0) + ##
    #scale_y_continuous(trans = "log10") +

  promise_size_visualization <-
    analyses$object_size_summary %>%
    ggplot(aes(`OBJECT TYPE`, `SIZE` + 1/(1024 * 1024), fill = `OBJECT TYPE`)) +
    geom_violin(draw_quantiles = c(0.25, 0.5, 0.75), scale = "width") +
    labs(title = "Distribution of object size by type",y = "SIZE + 1/(1024 * 1024) (MB) (log2 scale)") +
    scale_y_continuous(trans="log2",
                       breaks = trans_breaks("log2", function(x) 2^x),
                       labels = trans_format("log2", math_format(2^.x))) +
    scale_fill_gdocs() +
    guides(fill=FALSE)

  relative_size_visualization <-
    analyses$object_size_summary %>%
    ggplot(aes(`OBJECT TYPE`, `RELATIVE SIZE`, fill = `OBJECT TYPE`)) +
    geom_violin(draw_quantiles = c(0.25, 0.5, 0.75), scale = "width") +
    labs(title = "Distribution of relative object size by type") +
    scale_fill_gdocs() +
    guides(fill=FALSE)

  list("promise_count" = promise_count_visualization,
       "promise_size" = promise_size_visualization,
       "relative_count" = relative_count_visualization,
       "relative_size" = relative_size_visualization)

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
