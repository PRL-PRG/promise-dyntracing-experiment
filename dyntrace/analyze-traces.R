#!/usr/bin/env Rscript

library(fs)
library(stringr)
library(dplyr)
library(readr)
library(glue)

get_settings <- function() {
    settings <- list()
    args <- commandArgs(trailingOnly = TRUE)

    if(length(args) != 5) {
        stop(str_c("Incorrect number of command line arguments.\n",
                   "program <executable> <trace-input-dir> <trace-output-dir> <trace-state-dir> <trace-logs-dir>",
                   sep = " "))
    }

    settings$executable <- args[1]
    settings$trace_lazy_dir <- args[2]
    settings$trace_strict_dir <- args[3]
    settings$trace_state_dir <- args[4]
    settings$trace_logs_dir <- args[5]

    for(i in 2:5) {
        if(!dir_exists(args[i])) {
            stop(str_c(args[i], " does not exist"))
        }
    }

    settings$memory <- as.numeric(system("awk '/MemTotal/ {print $2}' /proc/meminfo",
                                         intern=TRUE)) * 1024

    processors <- as.numeric(system("lscpu | awk '/^CPU\\(s\\):/ { print $2 }'",
                                    intern=TRUE))


    settings$processors <- as.integer(processors / 1.125)

    settings$memory_limit <- settings$memory / 4

    settings
}

process_traces <- function(settings, processes, argument_filepath, joblog_filepath) {

    arg_glue <- function(str) {
        glue(str, .open = "(", .close = ")")
    }

    command <- "parallel"
    args <- c(arg_glue("--jobs (processes)"),
              "--files",
              "--bar",
              "--load 80%",
              arg_glue("--results (settings$trace_logs_dir)/packages/{}"),
              arg_glue("--joblog (joblog_filepath)"),
              arg_glue("(settings$executable) (settings$trace_lazy_dir)/{1}.trace (settings$trace_strict_dir)/{}.trace (settings$trace_state_dir)/{}.state"),
              arg_glue(":::: (argument_filepath)"))

    print(str_glue("{command} {str_c(args, collapse = ' ')} \n"))

    system2(command, args, stdout = "/dev/null")
}



main <- function() {
    settings <- get_settings()

    trace_files <-
        settings$trace_lazy_dir %>%
        dir_ls(path = . , all = FALSE, recursive = TRUE,
               type = "file", glob = "*.trace") %>%
        file_info() %>%
        select(path, size)

    min_size <- 0
    max_size <- 0
    one_megabyte <- 1024 * 1024
    one_gigabyte <- one_megabyte * 1024
    size_increment <- 512 * one_megabyte # 512 MB

    print(str_glue("Memory    : {settings$memory / one_gigabyte} GB",
                   "Processors: {settings$processors}",
                   "Files     : {nrow(trace_files)}",
                   .sep = "\n"))

    cat("\n**********************************************************\n\n")

    while(TRUE) {
        min_size <- max_size
        max_size <- max_size + size_increment
        processes <- min(settings$processors,
                         as.integer(settings$memory / (4 * max_size)))

        argument_filepath <- path(settings$trace_logs_dir,
                                  str_c(as.character(processes), ".txt"))

        joblog_filepath <- path(settings$trace_logs_dir, str_c(as.character(processes), "trace-summary", sep = "-"))

        if(processes == 1) {
            files <-
                trace_files %>%
                filter(size > min_size) %>%
                select(name = path, -size)

        } else {
            files <-
                trace_files %>%
                filter(size > min_size & size <= max_size) %>%
                select(name = path, -size)
        }

        files <-
            files %>%
            mutate(name = path(path_file(path_dir(name)),
                               path_ext_remove(path_file(name))))

        dir_create(path(settings$trace_strict_dir, path_dir(files$name)))

        dir_create(path(settings$trace_state_dir, path_dir(files$name)))

        write_tsv(files, argument_filepath, col_names = FALSE)

        print(str_glue("File count         : {nrow(files)}",
                       "File Size          : {min_size / one_megabyte} MB - {max_size / one_megabyte} MB",
                       "Parallel processes : {processes}", .sep = "\n"))

        process_traces(settings, processes, argument_filepath, joblog_filepath)

        if(processes == 1) {
            quit(0)
        }
    }
}

main()
