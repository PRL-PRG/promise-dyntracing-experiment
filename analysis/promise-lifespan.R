#!/usr/bin/env Rscript

## TODO - no good way in R to find path of current script
##        perhaps the only good solution is to create a
##        package for the analysis ?
source("analysis/utils.R")
source("analysis/analysis.R")

analyze_database <- function(database_filepath) {

    script = basename(file_path_sans_ext(database_filepath))
    db <- src_sqlite(database_filepath)

  call_counts_table <-
    tbl(db, "gc_trigger") %>%
    select(builtin_calls, special_calls, closure_calls) %>%
    mutate(total_calls = builtin_calls + special_calls + closure_calls) %>%
    collect() %>%
    rename(BUILTIN = builtin_calls, SPECIAL = special_calls, CLOSURE = closure_calls, TOTAL = total_calls) %>%
    gather(`CALL TYPE`, `CALL COUNT`) %>%
    mutate(`SCRIPT`=script)

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

    unforced_mortal_promise_table <-
      mortal_promise_table %>%
      filter(is.na(`1`))

    forced_mortal_promise_table <-
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
        "UNFORCED MORTAL PROMISE" =
          unforced_mortal_promise_table %>%
          nrow(),
        "FORCED MORTAL PROMISE" =
          forced_mortal_promise_table %>%
          nrow())

   lifespan_table <-
     mortal_promise_table %>%
     mutate(`GC CYCLES` = `2` - `0`) %>%
     group_by(`GC CYCLES`) %>%
     summarize(`PROMISE COUNT` = n()) %>%
     ungroup() %>%
     mutate(`SCRIPT`=script)

   required_lifespan_table <-
     forced_mortal_promise_table %>%
     ## the promise can only get garbage collected at the GC
     ## cycle after its last lookup, so we add 1 below
     mutate(`GC CYCLES` = `1` - `0` + 1) %>%
     group_by(`GC CYCLES`) %>%
     summarize(`PROMISE COUNT` = n()) %>%
     ungroup() %>%
     add_row(`GC CYCLES` = 0, `PROMISE COUNT` = nrow(unforced_mortal_promise_table)) %>%
     mutate(`SCRIPT`=script)

  unforced_promise_extra_lifespan <-
    unforced_mortal_promise_table %>%
    mutate(`GC CYCLES` = `2` - `0` - 1)

  extra_lifespan_table <-
     forced_mortal_promise_table %>%
     ## the promise can only get garbage collected at the GC
     ## cycle after its last lookup, so we subtract 1 below
     mutate(`GC CYCLES` = `2` - `1` - 1) %>%
     bind_rows(unforced_promise_extra_lifespan) %>%
     group_by(`GC CYCLES`) %>%
     summarize(`PROMISE COUNT` = n()) %>%
     ungroup() %>%
     filter(`GC CYCLES` != 0) %>%
     mutate(`SCRIPT`=script)

  list(lifespan = lifespan_table,
       required_lifespan = required_lifespan_table,
       extra_lifespan = extra_lifespan_table,
       counts = counts_table,
       call_counts = call_counts_table)
}

summarize_analyses <- function(analyses) {
  analyses$lifespan_summary <-
    analyses$lifespan %>%
    mutate(WEIGHT = `GC CYCLES` * `PROMISE COUNT`) %>%
    group_by(`SCRIPT`) %>%
    summarize(`MEAN GC CYCLES` = sum(WEIGHT)/sum(`PROMISE COUNT`),
              `MEDIAN GC CYCLES` = median(`GC CYCLES`),
              `MIN GC CYCLES` = min(`GC CYCLES`),
              `MAX GC CYCLES` = max(`GC CYCLES`),
              `PROMISE COUNT` = sum(`PROMISE COUNT`)) %>%
    ungroup()


  analyses$required_lifespan_summary <-
    analyses$required_lifespan %>%
    mutate(WEIGHT = `GC CYCLES` * `PROMISE COUNT`) %>%
    group_by(`SCRIPT`) %>%
    summarize(`MEAN GC CYCLES` = sum(WEIGHT)/sum(`PROMISE COUNT`),
              `MEDIAN GC CYCLES` = median(`GC CYCLES`),
              `MIN GC CYCLES` = min(`GC CYCLES`),
              `MAX GC CYCLES` = max(`GC CYCLES`),
              `PROMISE COUNT` = sum(`PROMISE COUNT`)) %>%
    ungroup()

  analyses$extra_lifespan_summary <-
    analyses$extra_lifespan %>%
    mutate(WEIGHT = `GC CYCLES` * `PROMISE COUNT`) %>%
    group_by(`SCRIPT`) %>%
    summarize(`MEAN GC CYCLES` = sum(WEIGHT)/sum(`PROMISE COUNT`),
              `MEDIAN GC CYCLES` = median(`GC CYCLES`),
              `MIN GC CYCLES` = min(`GC CYCLES`),
              `MAX GC CYCLES` = max(`GC CYCLES`),
              `PROMISE COUNT` = sum(`PROMISE COUNT`)) %>%
    ungroup()

  analyses$count_summary <-
    analyses$counts %>%
    select(-`SCRIPT`) %>%
    summarise_each(funs(sum))

  analyses
}

visualize_analyses <- function(analyses) {

  visualizations <- list()

  analyses$lifespan_summary %>%
    select(-`SCRIPT`, -`PROMISE COUNT`) %>%
    gather(`GC CYCLE STATISTICS TYPE`, `GC CYCLES`) %>%
    group_by(`GC CYCLE STATISTICS TYPE`) %>%
    do({

      name <- paste0("lifespan_",
                     stri_trans_tolower(
                       stri_replace_all_fixed(.$`GC CYCLE STATISTICS TYPE`[1],
                                              " ", "_")))
      visualizations[[name]] <<-
        ggplot(data=., aes(`GC CYCLE STATISTICS TYPE`, `GC CYCLES`)) +
        geom_violin(draw_quantiles = c(0.25, 0.5, 0.75), na.rm = TRUE) +
        labs(title = paste0("Lifespan - ",
                            stri_trans_totitle(.$`GC CYCLE STATISTICS TYPE`[1]),
                            " Distribution"))

      data.frame()
    })

  analyses$required_lifespan_summary %>%
    select(-`SCRIPT`, -`PROMISE COUNT`) %>%
    gather(`GC CYCLE STATISTICS TYPE`, `GC CYCLES`) %>%
    group_by(`GC CYCLE STATISTICS TYPE`) %>%
    do({

      name <- paste0("required_lifespan_",
                     stri_trans_tolower(
                       stri_replace_all_fixed(.$`GC CYCLE STATISTICS TYPE`[1],
                                              " ", "_")))
      visualizations[[name]] <<-
        ggplot(data=., aes(`GC CYCLE STATISTICS TYPE`, `GC CYCLES`)) +
        geom_violin(na.rm = TRUE) +
        geom_boxplot(width = .1) +
        labs(title = paste0("Required Lifespan - ",
                            stri_trans_totitle(.$`GC CYCLE STATISTICS TYPE`[1]),
                            " Distribution"))
      data.frame()
    })

  analyses$extra_lifespan_summary %>%
    select(-`SCRIPT`, -`PROMISE COUNT`) %>%
    gather(`GC CYCLE STATISTICS TYPE`, `GC CYCLES`) %>%
    group_by(`GC CYCLE STATISTICS TYPE`) %>%
    do({

      name <- paste0("extra_lifespan_",
                     stri_trans_tolower(
                       stri_replace_all_fixed(.$`GC CYCLE STATISTICS TYPE`[1],
                                              " ", "_")))
      visualizations[[name]] <<-
        ggplot(data=., aes(`GC CYCLE STATISTICS TYPE`, `GC CYCLES`)) +
        geom_violin(draw_quantiles = c(0.25, 0.5, 0.75), na.rm = TRUE) +
        labs(title = paste0("Extra Lifespan - ",
                            stri_trans_totitle(.$`GC CYCLE STATISTICS TYPE`[1]),
                            " Distribution"))
      data.frame()
    })

    visualizations$category_counts <-
      analyses$count_summary %>%
      gather(`PROMISE TYPE`, `PROMISE COUNT`) %>%
      ggplot(data = ., aes(`PROMISE TYPE`)) +
      geom_bar(aes(weight = `PROMISE COUNT`)) +
      scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                    labels = trans_format("log10", math_format(10^.x))) +
      labs(x = "PROMISE TYPE", y = "PROMISE COUNT (log10 scale)",
           title = "Promise Categories") +
      theme(axis.text.x=element_text(angle = 90, vjust=0.5))


  visualizations$call_count_distribution <-
    analyses$call_counts %>%
    ggplot(aes(`CALL TYPE`, `CALL COUNT`)) +
    geom_violin() +
    scale_y_continuous(labels = count_labels) +
    labs(title = "Distribution of number of calls to functions per GC trigger")

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
