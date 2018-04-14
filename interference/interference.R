#!/usr/bin/env Rscript

## TODO - no good way in R to find path of current script
##        perhaps the only good solution is to create a
##        package for the analysis ?
source("analysis/utils.R")
source("analysis/analysis.R")


main <- function() {

  arguments <- parse_program_arguments()
  # part is ignored
  part <- arguments$args[1]
  input_dir <- arguments$args[2]
  table_dir <- arguments$args[3]
  graph_dir <- arguments$args[4]

  summary <- read_csv(file.path(table_dir, "interference.csv"))

  groups <-
    summary %>%
    group_by(`VARIABLE NAME`, `CONTEXT TYPE`, `CONTEXT ID`,
             `LAZY CONTEXT TYPE`, `LAZY CONTEXT ID`,
             `EAGER CONTEXT TYPE`, `EAGER CONTEXT ID`) %>%
    summarize(`SCRIPT COUNT` = n(), `DYNAMIC COUNT` = sum(`COUNT`))

  counts <- tibble(TYPE = c("SCRIPT", "STATIC", "DYNAMIC"),
                   COUNT = c(nrow(distinct(summary, `SCRIPT`)),
                             nrow(groups), sum(groups[["DYNAMIC COUNT"]])))

  write_csv(groups, file.path(table_dir, "groups.csv"))
  write_csv(counts, file.path(table_dir, "counts.csv"))
}


main()
