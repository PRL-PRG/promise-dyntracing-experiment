#!/usr/bin/env Rscript

library(tidyverse)
library(stringr)

make_annotater <- function(database_filepath) {

  db <- src_sqlite(database_filepath)

  calls_table <-
    tbl(db, "calls") %>%
    rename(expression = call_expression)

  promises_table <- tbl(db, "promises")

  variables_table <-
    tbl(db, "variables") %>%
    rename(expression = name)

  functions_table <-
    tbl(db, "functions") %>%
    rename(expression = definition)

  lookup <- function(table, aid) {
    table %>%
      filter(id == aid) %>%
      collect() %>%
      slice(1) %>%
      pull(expression)
      #ifelse(. )
  }

  indentation <- 0;

  paste_trace <- function(trace) {

    if(trace[1] %in% c("prf", "spf", "buf", "clf", "prj", "fnj"))
      indentation <<- indentation - 1

    text <-
      paste(
        paste(rep(" ", indentation * 4), collapse = ""),
        paste(trace, collapse = " ■ "),
        sep = "")

    if(trace[1] %in% c("prb", "spb", "bub", "clb"))
      indentation <<- indentation + 1

    text
  }

  annotate <-
    function(trace) {

      if(trace[1] %in% c("prc", "prb", "prf", "prv", "pre"))
        c(trace,
          sanitize(lookup(promises_table, trace[2])))
      else if(trace[1] %in% c("bub", "buf", "spb", "spf", "clb", "clf"))
        c(trace,
          sanitize(lookup(functions_table, trace[2])),
          sanitize(lookup(calls_table, trace[3])))
      else if(trace[1] %in% c("enl", "end", "ena", "enr"))
        c(trace,
          sanitize(lookup(variables_table, trace[3])))
      else if(trace[1] %in% c("enc", "prj", "fnj"))
        trace
      else
        c(trace, "NOT HANDLED")

      ## if(opcode %in% c("spb", "spe", "bub", "bue", "clb", "cle"))
      ##   lookup(calls_table, id)
      ## else if(opcode %in% c("cre", "ent", "ext", "val"))
      ##   lookup(promises_table, id)
      ## else if(opcode %in% c("rea", "rem", "asn", "def"))
      ##   lookup(variables_table, id)
      ## else
      ##   paste0("OPCODE ", opcode, " NOT HANDLED")
    }

  list(annotate = annotate,
       paste_trace = paste_trace)
}

sanitize <- function(annotation) {
  annotation %>%
    str_replace_all("\\s*\\{\\s*", "{") %>%
    str_replace_all("\\s+\\}", "}") %>%
    str_replace_all("\n\\s*","; ")
}

parse_trace <- function(trace) {
  str_split(trace, " ")[[1]]
}

main <- function() {
  args = commandArgs(trailingOnly=TRUE)
  if(length(args) != 3) {
    print("trace_filepath database_filepath annotated_filepath")
    exit()
  }
  trace_filepath <- args[1]
  database_filepath <- args[2]
  annotated_filepath <- args[3]

  trace_file <- file(trace_filepath, "rt")
  annotated_file <- file(annotated_filepath, "wt")
  temp <- make_annotater(database_filepath)
  annotate <- temp$annotate
  paste_trace <- temp$paste_trace

  while(TRUE) {
    line <- readLines(trace_file, n = 1)
    #print(line)
    if(length(line) == 0) break
    annotated_line <- paste_trace(annotate(parse_trace(line)))
    #print(annotated_line)
    writeLines(annotated_line, annotated_file)
  }

  close(trace_file)
  close(annotated_file)
  ## f <- read_delim("./interference.trace", " ", col_names = c("OPCODE", "ID"))

  ## f %>%
  ##   mutate(ANNOTATION = sanitize(annotater(OPCODE, ID))) %>%
  ##   write_delim(annotated_filepath, " ")
}


main()
