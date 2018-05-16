#!/usr/bin/env Rscript

## TODO - no good way in R to find path of current script
##        perhaps the only good solution is to create a
##        package for the analysis ?
source("analysis/utils.R")
source("analysis/analysis.R")

summarize_analyses <- function(analyses) {

  promise_count <- sum(analyses$`promise-type`$count)

  object_count_size <-
    analyses$`object-count-size` %>%
    select(-vignette, -package) %>%
    add_row(object_type = "Promise", count=promise_count, size=promise_count * 56) %>%
    group_by(object_type) %>%
    summarize(count = sum(count), size = sum(size)) %>%
    ungroup() %>%
    mutate(relative_size = 100 * size/sum(size),
           relative_count= 100 * count/sum(count),
           group_type = ifelse(object_type == "Promise", "Promises", "Other Objects"))

  total_object_count <-
    analyses$`object-count-size` %>%
    pull(count) %>%
    sum()

  total_object_size <-
    analyses$`object-count-size` %>%
    pull(size) %>%
    sum()

  print(
    list("object_count_size" = object_count_size,
         "aggregate" = tibble(total_object_count = total_object_count,
                              total_object_size = total_object_size)))

}

visualize_analyses <- function(analyses) {

  total_object_count <- analyses$aggregate$total_object_count

  object_count_visualization <-
    analyses$object_count_size %>%
    rename("Object type" = object_type) %>%
    ggplot(aes(group_type, weight = relative_count, fill=`Object type`)) +
    geom_bar() +
    scale_y_continuous(sec.axis = sec_axis(~ . * total_object_count / 100,
                                           labels = count_labels),
                       labels = percent_labels) +
    labs(y ="Count (%)") +
    labs(x = NULL) +
    guides(title = NULL) +
    scale_fill_gdocs() +
    theme(legend.position = "bottom")

  total_object_size <- analyses$aggregate$total_object_size

  object_size_visualization <-
    analyses$object_count_size %>%
    rename("Object type" = object_type) %>%
    ggplot(aes(group_type, weight = relative_size, fill=`Object type`)) +
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

latex_analyses <-
  function(analyses) {

    relative_count_size <-
      analyses$object_count_size %>%
      group_by(group_type) %>%
      summarize(relative_size = sum(relative_size),
                relative_count = sum(relative_count))

    count_size <-
      analyses$object_count_size %>%
      group_by(group_type) %>%
      summarize(size = sum(size),
                count = sum(count))

    append(to_named_values(count_size, "GROUP TYPE"),
           to_named_values(relative_count_size, "GROUP TYPE"))

  }

main <-
  function() {
    analyzer <-
      create_analyzer("Promise Memory Usage Analysis",
                      combine_analyses,
                      summarize_analyses,
                      visualize_analyses,
                      latex_analyses)
    drive_analysis(analyzer)
  }

main()
warnings()
