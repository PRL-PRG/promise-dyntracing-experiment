library(processx)
library(fs)
library(tidyr)
library(dplyr)
library(purrr)
library(magrittr)
library(optparse)
library(stringr)
library(readr)
library(tibble)

options(tibble.width = Inf)

info <- function(...) cat((paste0(...)))


nth_element <- function(n) {
    function(vs) {
        map_chr(vs, function(v) v[n])
    }
}

last_element <- function(vs) {
    map_chr(vs, function(v) v[length(v)])
}

count_lines_of_code <- function(path, languages, list_file = FALSE, working_dirpath = NULL) {

    path <- path_abs(path)

    cloc_output <- run(command = "sh",
                       args = c("-c",
                                str_c("cloc",
                                      "--by-file",
                                      "--quiet",
                                      "--hide-rate",
                                      "--csv",
                                      "--csv-delimiter='|'",
                                      str_c("--include-lang='",
                                            str_c(languages, collapse = ","),
                                            "'"),
                                      if(list_file)
                                          str_c("--list-file=", path)
                                      else
                                          path,
                                      sep = " ")),
                       echo_cmd = TRUE,
                       wd = working_dirpath)

    if(is.na(cloc_output$status) | cloc_output$status != 0) {
        print(cloc_output$stderr)
        stop("The cloc process exited with a non zero exit status")
    }
    #\x1f
    if(cloc_output$stdout == "") {
        tibble(filename = NA, language = NA, code = NA, blank = NA, comment = NA)
    }
    else {
        read_delim(file = cloc_output$stdout, delim="|", col_names = TRUE) %>%
            filter(language != "SUM") %>%
            select(filename, language, blank, comment, code) %>%
            mutate(code = as.double(code),
                   blank = as.double(blank),
                   comment = as.double(comment))
    }
}


count_file_lines_of_code <- function(filepath, languages = "R") {
    count_lines_of_code(filepath, languages) %>%
        select(code, blank, comment)
}


analyze_corpus <- function(settings) {

    generate_script_filepath <- function(input_corpus_dirpath, filepaths) {
        filepaths <-
            path(filepaths,
                 ext = "R")

        if_else(file_exists(path(input_corpus_dirpath, filepaths)),
                filepaths,
                path_ext_set(path_ext_remove(filepaths), "r"))
    }

    compute_status <- function(begin, finish, error, noerror) {
        if_else(begin & finish & noerror, "SUCCESSFUL",
        if_else(begin & finish & error, "FAILED",
        if_else(begin & !finish, "RUNNING",
        if_else(!begin, "UNPROCESSED",
               "INVALID"))))
    }

    info("=> Generating valid scripts\n")

    valid_scripts <-
        read_csv(settings$input_scanned_valid_script_dirpath, col_names = "filepath") %>%
        mutate(filepath = generate_script_filepath(settings$input_corpus_dirpath,
                                                   filepath))

    testthat_dirpaths <-
        valid_scripts %>%
        mutate(filepath = path_ext_remove(filepath)) %>%
        filter(path_file(filepath) == "testthat") %>%
        filter(dir_exists(path(settings$input_corpus_dirpath, filepath)))

    valid_scripts <-
        bind_rows(valid_scripts, testthat_dirpaths)

    valid_scripts_filepath <-
        path(settings$output_corpus_data_dirpath, "valid_scripts", ext = "csv")

    write_csv(valid_scripts, valid_scripts_filepath, col_names = FALSE)

    info("=> Generated valid scripts in '", valid_scripts_filepath, "'\n")

    info("=> Counting program lines of code\n")

    program_code_count <-
        count_lines_of_code(valid_scripts_filepath, "R",
                            TRUE, settings$input_corpus_dirpath) %>%
        mutate(path_components = path_split(filename)) %>%
        mutate(script_name = last_element(path_components),
               script_type = nth_element(2)(path_components),
               package_name = nth_element(1)(path_components),
               language = "R") %>%
        select(package_name, script_type, script_name, language, code, blank, comment)

    info("=> Counted program lines of code\n")

    info("=> Generating package list\n")

    program_package_names <-
        program_code_count %>%
        pull(package_name) %>%
        unique() %>%
        sort()

    package_filepath <-
        path(settings$output_corpus_data_dirpath, "packages", ext = "csv")

    write_csv(tibble(package_name = program_package_names),
              package_filepath, col_names = FALSE)

    info("=> Generated package list in '", package_filepath, "'\n")

    sanitize_script_type <-
        function(script_type) {
            if_else(script_type %in% c("doc", "tests", "examples", "src", "R"),
                    script_type,
                    "other")
        }

    info("=> Counting total package code and line'\n")

    total_package_code_count <-
        count_lines_of_code("", c("C++", "C", "R"), FALSE,
                            settings$input_package_code_dirpath) %>%
        mutate(language = if_else(language == "R", "R", "C/C++")) %>%
        mutate(path_components = path_split(filename)) %>%
        mutate(package_name = nth_element(1)(path_components),
               script_type = nth_element(2)(path_components),
               script_name = path_ext_remove(last_element(path_components))) %>%
        select(package_name, script_type, script_name, language, code, blank, comment) %>%
        mutate(script_type = sanitize_script_type(script_type)) %>%
        group_by(package_name, script_type, language) %>%
        summarize(script_count = n(),
                  code = sum(code),
                  blank = sum(blank),
                  comment = sum(comment))

    info("=> Counted total package code and line'\n")


    info("=> Counting package lines of code\n")

    package_code_count <-
        count_lines_of_code(package_filepath, c("C++", "C", "R"), TRUE,
                            settings$input_package_code_dirpath) %>%
        mutate(language = if_else(language == "R", "R", "C/C++")) %>%
        mutate(path_components = path_split(filename)) %>%
        mutate(package_name = map_chr(path_components, function(path_component) path_component[1]),
               script_type = map_chr(path_components, function(path_component) path_component[2]),
               script_name = path_ext_remove(path_file(filename))) %>%
        select(package_name, script_type, script_name, language, code, blank, comment) %>%
        filter(!(script_type %in% c("doc", "tests", "examples", "data"))) %>%
        print()

    info("=> Counted package lines of code.\n")

    total_code_count <-
        package_code_count %>%
        bind_rows(program_code_count) %>%
        group_by(script_type, language) %>%
        summarize(code = sum(code, na.rm = TRUE),
                  blank = sum(blank, na.rm = TRUE),
                  comment = sum(comment, na.rm = TRUE)) %>%
        ungroup()

    simplified_total_code_count <-
        total_code_count %>%
        mutate(script_type = if_else(script_type %in% c("doc", "tests", "examples", "src", "R"), script_type, "other")) %>%
        group_by(script_type, language) %>%
        summarize(code = sum(code, na.rm = TRUE),
                  blank = sum(blank, na.rm = TRUE),
                  comment = sum(comment, na.rm = TRUE)) %>%
        ungroup()

    r_code_count <-
        simplified_total_code_count %>%
        filter(language == "R")

    c_code_count <-
        simplified_total_code_count %>%
        filter(language == "C/C++")

    simplified_total_code_count <-
        simplified_total_code_count %>%
        add_row(script_type = "total", language = "R",
                code = sum(r_code_count$code),
                blank = sum(r_code_count$blank),
                comment = sum(r_code_count$comment)) %>%
        add_row(script_type = "total", language = "C/C++",
                code = sum(c_code_count$code),
                blank = sum(c_code_count$blank),
                comment = sum(c_code_count$comment))

    return(list(total_package_code_count = total_package_code_count,
                program_code_count = program_code_count,
                package_code_count = package_code_count,
                total_code_count = total_code_count,
                simplified_total_code_count = simplified_total_code_count))

    ##     ## valid_scripts %>%
    ##     ## pmap_dfr(function(filepath, package_name, script_type, script_name) {
    ##     ##          count_file_lines_of_code(filepath)
    ##     ##      })

    ## ## program_code_count_data <-
    ## ##     bind_cols(valid_scripts, program_code_count_data) %>%
    ## ##     print()

    ## program_code_count_data <-
    ##     count_lines_of_code(settings$input_corpus_dirpath, "R") %>%
    ##     filter(language %in% "R") %>%
    ##     mutate(script_name = path_ext_remove(path_file(filename)),
    ##            package_name = path_file(path_dir(path_dir(filename))),
    ##            script_type = path_file(path_dir(filename))) %>%
    ##     select(package_name, script_type, script_name, code, blank, comment) %>%
    ##     filter(str_sub(script_name, 1, 11) == "__wrapped__")

    traced_table <-
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
        select(-BEGIN, -FINISH, -ERROR, -NOERROR)

    status_table <-
        traced_table %>%
        select(status, script_type) %>%
        group_by(status, script_type) %>%
        summarize(script_type_count = n()) %>%
        ungroup() %>%
        spread(status, script_type_count, fill = 0)


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
               -CONFIGURATION) %>%
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
}

serialize_analyses <- function(analyses, settings) {

    imap(analyses,
         function(df, name) {
             write_csv(df,
                       path(settings$output_corpus_data_dirpath,
                            name,
                            ext = "csv"))
         })

    path(settings$output_corpus_data_dirpath,
         names(analyses),
         ext = "csv")
}

parse_program_arguments <- function() {

    usage <- "%prog input-corpus-dirpath input-raw-data-dirpath output-corpus-data-dirpath"

    description <- paste(
        "input-scanned-valid-script-filepath filepath for valid scripts",
        "input-corpus-dirpath                directory containing corpus files",
        "input-raw-data-dirpath              directory containing raw analysis data",
        "input-package-source-dirpath        directory containing raw analysis data",
        "output-corpus-data-dirpath          directory in which the output files will be stored",
        sep = "\n")

    option_parser <- OptionParser(usage = usage,
                                  description = description,
                                  add_help_option = TRUE,
                                  option_list = list())

    arguments <- parse_args2(option_parser)

    list(input_scanned_valid_script_dirpath = arguments$args[1],
         input_corpus_dirpath = arguments$args[2],
         input_raw_data_dirpath = arguments$args[3],
         input_package_code_dirpath = arguments$args[4],
         output_corpus_data_dirpath = arguments$args[5])
}


main <- function() {
    settings <- parse_program_arguments()
    print(settings)
    analyses <- analyze_corpus(settings)
    serialize_analyses(analyses, settings)
}

main()
