#!/usr/bin/env Rscript

## TODO - no good way in R to find path of current script
##        perhaps the only good solution is to create a
##        package for the analysis ?
source("analysis/utils.R")
source("analysis/analysis.R")

analyze_database <- function(database_filepath) {

    script = basename(file_path_sans_ext(database_filepath))
    db <- src_sqlite(database_filepath)

    promise_lifecycle_table <-
      tbl(db, "promise_lifecycle") %>%
      group_by(promise_id, event_type) %>%
      summarize(gc_trigger_counter = max(gc_trigger_counter)) %>%
      ungroup() %>%
      collect()

  ## if the table contains 0 entries, then we return an empty list.
  if(nrow(promise_lifecycle_table) == 0)
    return (list())

  promise_lifecycle_table <-
    promise_lifecycle_table %>%
    spread(event_type, gc_trigger_counter) %>%
    mutate(`0` = if (exists('0', where = .)) `0` else NA) %>%
    mutate(`1` = if (exists('1', where = .)) `1` else NA) %>%
    mutate(`2` = if (exists('2', where = .)) `2` else NA)

    mortal_promise_table <-
      promise_lifecycle_table %>%
      filter(!is.na(`2`) & !is.na(`0`))

    unforced_promise_table <-
      mortal_promise_table %>%
      filter(is.na(`1`))

    mortal_promise_table <-
      mortal_promise_table %>%
      filter(!is.na(`1`))

  counts_table <-
      tibble(
        "SCRIPT" = script,
        "TOTAL PROMISE" =
          promise_lifecycle_table %>%
          nrow(),
        "FOREIGN PROMISE" =
          promise_lifecycle_table %>%
          filter(is.na(`0`)) %>%
          nrow(),
        "IMMORTAL PROMISE" =
          promise_lifecycle_table %>%
          filter(!is.na(`0`) & is.na(`2`)) %>%
          nrow(),
        "MORTAL PROMISE" =
          mortal_promise_table %>%
          nrow(),
        "UNFORCED PROMISE" =
          unforced_promise_table %>%
          nrow())

   alive_table <-
     mortal_promise_table %>%
     mutate(`GC CYCLES` = `2` - `0`) %>%
     group_by(`GC CYCLES`) %>%
     summarize(`PROMISE COUNT` = n()) %>%
     mutate(`SCRIPT`=script)

   indispensable_table <-
     mortal_promise_table %>%
     mutate(`GC CYCLES` = `1` - `0`) %>%
     group_by(`GC CYCLES`) %>%
     summarize(`PROMISE COUNT` = n()) %>%
     mutate(`SCRIPT`=script)

   dispensable_table <-
     mortal_promise_table %>%
     mutate(`GC CYCLES` = `2` - `1` - 1) %>%
     group_by(`GC CYCLES`) %>%
     summarize(`PROMISE COUNT` = n()) %>%
     mutate(`SCRIPT`=script)

  unforced_table <-
    unforced_promise_table %>%
    mutate(`GC CYCLES` = `2` - `0`) %>%
    group_by(`GC CYCLES`) %>%
    summarize(`PROMISE COUNT` = n()) %>%
    mutate(`SCRIPT` = script)

  list(alive = alive_table,
       indispensable = indispensable_table,
       dispensable = dispensable_table,
       counts = counts_table,
       unforced = unforced_table)
}

combine_analyses <- function(tables_acc, tables) {
  list(alive = bind_rows(tables_acc$alive, tables$alive),
       indispensable = bind_rows(tables_acc$indispensable, tables$indispensable),
       dispensable = bind_rows(tables_acc$dispensable, tables$dispensable),
       counts = bind_rows(tables_acc$counts, tables$counts),
       unforced = bind_rows(tables_acc$unforced, tables$unforced))
}

summarize_analyses <- function(analyses) {
  analyses[["alive-summarized"]] <-
    analyses$alive %>%
    group_by(`GC CYCLES`) %>%
    summarize(`PROMISE COUNT` = sum(`PROMISE COUNT`))

  analyses[["indispensable-summarized"]] <-
    analyses$indispensable %>%
    group_by(`GC CYCLES`) %>%
    summarize(`PROMISE COUNT` = sum(`PROMISE COUNT`))

  analyses[["dispensable-summarized"]] <-
    analyses$dispensable %>%
    group_by(`GC CYCLES`) %>%
    summarize(`PROMISE COUNT` = sum(`PROMISE COUNT`))

  analyses[["unforced-summarized"]] <-
    analyses$unforced %>%
    group_by(`GC CYCLES`) %>%
    summarize(`PROMISE COUNT` = sum(`PROMISE COUNT`))

  analyses[["counts-summarized"]] <-
    analyses$counts %>%
    select(-`SCRIPT`) %>%
    summarise_each(funs(sum))

  analyses
}

visualize_analyses <- function(analyses) {
  visualizations = list(
    alive =
      analyses$alive %>%
      group_by(`GC CYCLES`) %>%
      ## WARN: I do `+ 1` below because log(0) will result in
      ##       incorrect plot. The minimum value should be 1
      ##       for log plot to have any meaning.
      summarize(`PROMISE COUNT` = sum(`PROMISE COUNT`) + 1) %>%
      ggplot(data = ., aes(`GC CYCLES`)) +
      geom_bar(aes(weight=`PROMISE COUNT`)) +
      scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                    labels = trans_format("log10", math_format(10^.x))) +
      scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x),
                    labels = trans_format("log10", math_format(10^.x))) +
      labs(x = "GC CYCLES", y = "PROMISE COUNT (log10 scale)",
           title = "Alive Promise Distribution"),

    indispensable =
      analyses$indispensable %>%
      group_by(`GC CYCLES`) %>%
      ## WARN: I do `+ 1` below because log(0) will result in
      ##       incorrect plot. The minimum value should be 1
      ##       for log plot to have any meaning.
      summarize(`PROMISE COUNT` = sum(`PROMISE COUNT`) + 1) %>%
      ggplot(data = ., aes(`GC CYCLES`)) +
      geom_bar(aes(weight=`PROMISE COUNT`)) +
      scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                    labels = trans_format("log10", math_format(10^.x))) +
      scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x),
                    labels = trans_format("log10", math_format(10^.x))) +
      labs(x = "GC CYCLES", y = "PROMISE COUNT (log10 scale)",
           title = "Indispensable Promise Distribution"),

    dispensable =
      analyses$dispensable %>%
      group_by(`GC CYCLES`) %>%
      ## WARN: I do `+ 1` below because log(0) will result in
      ##       incorrect plot. The minimum value should be 1
      ##       for log plot to have any meaning.
      summarize(`PROMISE COUNT` = sum(`PROMISE COUNT`) + 1) %>%
      ggplot(data = ., aes(`GC CYCLES`)) +
      geom_bar(aes(weight=`PROMISE COUNT`)) +
      scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                    labels = trans_format("log10", math_format(10^.x))) +
      scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x),
                    labels = trans_format("log10", math_format(10^.x))) +
      labs(x = "GC CYCLES", y = "PROMISE COUNT (log10 scale)",
           title = "Dispensable Promise Distribution"),

    unforced =
      analyses$unforced %>%
      group_by(`GC CYCLES`) %>%
      ## WARN: I do `+ 1` below because log(0) will result in
      ##       incorrect plot. The minimum value should be 1
      ##       for log plot to have any meaning.
      summarize(`PROMISE COUNT` = sum(`PROMISE COUNT`) + 1) %>%
      ggplot(data = ., aes(`GC CYCLES`)) +
      geom_bar(aes(weight=`PROMISE COUNT`)) +
      scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                    labels = trans_format("log10", math_format(10^.x))) +
      scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x),
                    labels = trans_format("log10", math_format(10^.x))) +
      labs(x = "GC CYCLES", y = "PROMISE COUNT (log10 scale)",
           title = "Unforced Promise Distribution"),

    counts =
      analyses[["counts-summarized"]] %>%
      gather(`PROMISE TYPE`, `PROMISE COUNT`) %>%
      ggplot(data = ., aes(`PROMISE TYPE`)) +
      geom_bar(aes(weight = `PROMISE COUNT`)) +
      scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                    labels = trans_format("log10", math_format(10^.x))) +
      labs(x = "PROMISE TYPE", y = "PROMISE COUNT (log10 scale)",
           title = "Promise Categories") +
      theme(axis.text.x=element_text(angle = 90, vjust=0.5))
    )

  visualizations
}


main <- function() {
  drive_analysis("Promise Lifespan Analysis",
                 analyze_database,
                 combine_analyses,
                 summarize_analyses,
                 export_as_tables,
                 import_as_tables,
                 visualize_analyses,
                 export_as_images)
}

main()

export_visualizations <- function(visualizations, graph_dir) {
  visualizations$alive %>%
    ggsave(plot = ., filename=file.path(graph_dir, "alive.png"))

  visualizations$indispensable %>%
      ggsave(plot = ., filename=file.path(graph_dir, "indispensable.png"))

  visualizations$dispensable %>%
    ggsave(plot = ., filename=file.path(graph_dir, "dispensable.png"))

  visualizations$count %>%
    ggsave(plot = ., filename=file.path(graph_dir, "count.png"))

  visualizations$unforced %>%
    ggsave(plot = ., filename=file.path(graph_dir, "unforced.png"))
}

export_analyses <- function(datatable, table_dir) {
  datatable$alive %>%
    write_csv(file.path(table_dir, "alive.csv"))

  datatable$alive %>%
    group_by(`GC CYCLES`) %>%
    summarize(`PROMISE COUNT` = sum(`PROMISE COUNT`)) %>%
    write_csv(file.path(table_dir, "alive-summarized.csv"))

  datatable$indispensable %>%
    write_csv(file.path(table_dir, "indispensable.csv"))

  datatable$indispensable %>%
    group_by(`GC CYCLES`) %>%
    summarize(`PROMISE COUNT` = sum(`PROMISE COUNT`)) %>%
    write_csv(file.path(table_dir, "indispensable-summarized.csv"))

  datatable$dispensable %>%
    write_csv(file.path(table_dir, "dispensable.csv"))

  datatable$dispensable %>%
    group_by(`GC CYCLES`) %>%
    summarize(`PROMISE COUNT` = sum(`PROMISE COUNT`)) %>%
    write_csv(file.path(table_dir, "dispensable-summarized.csv"))

  datatable$unforced %>%
    write_csv(file.path(table_dir, "unforced.csv"))

  datatable$unforced %>%
    group_by(`GC CYCLES`) %>%
    summarize(`PROMISE COUNT` = sum(`PROMISE COUNT`)) %>%
    write_csv(file.path(table_dir, "unforced-summarized.csv"))

  datatable$counts %>%
    write_csv(file.path(table_dir, "counts.csv"))

  datatable$counts %>%
    select(-`SCRIPT`) %>%
    summarise_each(funs(sum)) %>%
    write_csv(file.path(table_dir, "counts-summarized.csv"))

}
