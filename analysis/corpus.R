library(processx)
library(fs)
library(tidyr)
library(dplyr)
library(purrr)
library(magrittr)
library(optparse)

options(tibble.width = Inf)

count_lines_of_code <- function(settings) {

    cloc_output <- run(command = "cloc",
                       args = c("--by-file",
                                "--quiet",
                                "--hide-rate",
                                "--csv",
                                settings$input_corpus_dirpath))
    print(cloc_output$stderr)
    if(is.na(cloc_output$status) | cloc_output$status != 0) {
        stop("The cloc process exited with a non zero exit status")
    }

    read.csv(text = cloc_output$stdout, stringsAsFactors = FALSE) %>%
        filter(language == "R") %>%
        mutate(script_name = path_ext_remove(path_file(filename)),
               package_name = path_file(path_dir(path_dir(filename))),
               script_type = path_file(path_dir(filename))) %>%
        select(package_name, script_type, script_name, code, blank, comment)
}

analyze_corpus <- function(settings) {

    generate_valid_script_filepath <- function(script_name) {
    }

    compute_status <- function(begin, finish, error, noerror) {
        if_else(begin & finish & noerror, "SUCCESSFUL",
        if_else(begin & finish & error, "FAILED",
        if_else(begin & !finish, "RUNNING",
        if_else(!begin, "UNPROCESSED",
               "INVALID"))))
    }


    compute_functions_size <- function(path) {
        path %>%
            dir_ls() %>%
            file_info() %>%
            pull(size) %>%
            sum()
    }

    count_data <- count_lines_of_code(settings)

    corpus_table <-
        settings$input_raw_data_dirpath %>%
        dir_ls() %>%
        dir_ls() %>%
        dir_ls() %>%
        dir_ls() %>%
        file_info() %>%
        select(traced_filepath = path, size) %>%
        mutate(package_name = path_file(path_dir(path_dir(path_dir(traced_filepath)))),
               script_type = path_file(path_dir(path_dir(traced_filepath))),
               script_name = path_file(path_dir(traced_filepath)),
               traced_filename = path_file(traced_filepath)) %>%
        select(-traced_filepath) %>%
        spread(traced_filename, size) %>%
        mutate(status = compute_status(!is.na(BEGIN), !is.na(FINISH),
                                       !is.na(ERROR), !is.na(NOERROR))) %>%
        select(-BEGIN, -FINISH, -ERROR, -NOERROR) %>%
        mutate(functions = compute_functions_size(path(settings$input_raw_data_dirpath,
                                                       package_name,
                                                       script_type,
                                                       script_name,
                                                       "functions"))) %>%
        left_join(count_data, by = c("package_name", "script_type", "script_name"))

    status_table <-
        corpus_table %>%
        group_by(status) %>%
        group_by(status, script_type) %>%
        summarize(script_type_count = n()) %>%
        ungroup() %>%
        spread(status, script_type_count, fill = 0)


    code_table <-
        corpus_table %>%
        filter(status == "SUCCESSFUL") %>%
        group_by(script_type) %>%
        summarize(code = sum(code), blank = sum(blank), comment = sum(blank)) %>%
        ungroup()

    code_table_total <-
        code_table %>%
        select(-script_type) %>%
        summarize_all(sum) %>%
        mutate(script_type = "total")

    code_table <-
        bind_rows(code_table, code_table_total)


    size_table <-
        corpus_table %>%
        filter(status == "SUCCESSFUL") %>%
        select(-code, -blank, -comment, -status,
               -script_name, -package_name,
               -CONFIGURATION, -ENVVAR) %>%
        gather(filename, size, -script_type) %>%
        group_by(script_type, filename) %>%
        summarize(size = sum(size)) %>%
        ungroup() %>%
        spread(script_type, size) %>%
        mutate(total = doc + examples + tests) %>%
        arrange(desc(total))

    size_table_total <-
        size_table %>%
        select(-filename) %>%
        summarize_all(sum) %>%
        mutate(filename = "total")

    size_table <-
        size_table %>%
        bind_rows(size_table_total) %>%
        mutate_if(
            is.numeric,
            function(col) {
                class(col) <- list("fs_bytes")
                col
            })

    list(corpus_table = corpus_table,
         status_table = status_table,
         code_table = code_table,
         size_table = size_table)

    status_table
}



parse_program_arguments <- function() {
    usage <- "%prog reduced-output-dirpath combined-output-dirpath"
    description <- paste(
        "reduced-output-dirpath    directory containing reduced data files (scanned recursively)",
        "combined-output-dirpath   directory to which combined data will be exported",
        sep = "\n")


    option_parser <- OptionParser(usage = usage,
                                  description = description,
                                  add_help_option = TRUE,
                                  option_list = list())

    arguments <- parse_args2(option_parser)

    list(input_corpus_dirpath = arguments$args[1],
         input_raw_data_dirpath = arguments$args[2],
         output_corpus_data_dirpath = arguments$args[3])
}


main <- function() {
    settings <- parse_program_arguments()
    print(settings)
    analyze_corpus(settings)
}

main()
