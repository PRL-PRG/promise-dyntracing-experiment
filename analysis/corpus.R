library(promisedyntracer)
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
    if(n >= 0) {
        function(vs) {
            map_chr(vs, function(v) v[n])
        }
    }
    else {
        function(vs) {
            k <- -n - 1
            map_chr(vs, function(v) v[length(v) - k])
        }
    }
}


last_element <- function(vs) {
    map_chr(vs, function(v) v[length(v)])
}


count_installed_package_rm_files <- function() {
    build_vignettes <- function(package) {

        vignettes <- tools::pkgVignettes(package, source=TRUE)

        ## if there are vignettes and there are no vignette sources,
        ## then compile the vignettes to sources. This compilation
        ## will result in .R files in the doc directory of package
        ## and will be picked up by the next step of the program
        if (length(vignettes$docs) != 0 &&
            length(vignettes$sources) == 0) {

            tools::checkVignettes(settings$package,
                                  find.package(settings$package)[1],
                                  tangle = TRUE,
                                  weave = FALSE,
                                  workdir = "src")
        }
    }
}

count_lines_of_code <- function(path, languages, list_file = FALSE,
                                working_dirpath = NULL) {

    if(list_file) {
        path <- path_abs(path)
    }

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

sanitize_script_type <- function(script_type) {
    if_else(script_type == "doc", "Vignettes",
    if_else(script_type == "tests", "Tests",
    if_else(script_type == "examples", "Examples",
    if_else(script_type == "src",  "Source/C-C++",
    if_else(script_type == "R", "Source/R",
            "Other")))))
}

generate_script_filepath <- function(input_corpus_dirpath, filepaths) {
    filepaths <-
        path(filepaths,
             ext = "R")

    if_else(file_exists(path(input_corpus_dirpath, filepaths)),
            filepaths,
            path_ext_set(path_ext_remove(filepaths), "r"))
}

analyze_installed_corpus <- function(settings) {

    get_corpus <- function(dirpath, languages = "R") {
        relative_path <- "."

        relative_path_offset <- length(path_split(relative_path)[[1]])

        package_code_count <-
            count_lines_of_code(relative_path, languages, FALSE, dirpath) %>%
            mutate(language = if_else(language == "R", "R", "C/C++")) %>%
            mutate(path_components = path_split(filename)) %>%
            mutate(package_name = nth_element(1 + relative_path_offset)(path_components),
                   script_type = nth_element(2 + relative_path_offset)(path_components),
                   script_name = path_ext_remove(last_element(path_components))) %>%
            select(package_name, script_type, script_name, language, code, blank, comment)
    }

    info("=> Generating test corpus\n")

    test_corpus <- get_corpus(settings$package_test_dirpath)

    info("=> Generating example corpus\n")

    example_corpus <- get_corpus(settings$package_example_dirpath)

    info("=> Generating vignette corpus\n")

    vignette_corpus <- get_corpus(settings$package_vignette_dirpath)

    info("=> Generating source corpus\n")

    source_corpus <-
        get_corpus(settings$package_src_dirpath, languages = c("C++", "C", "R")) %>%
        filter(script_type %in% c("R", "src"))

    installed_corpus <-
        bind_rows(list(test_corpus, example_corpus, vignette_corpus, source_corpus)) %>%
        mutate(script_type = sanitize_script_type(script_type)) %>%
        filter(package_name %in% installed.packages()[,1]) %>%
        group_by(package_name, script_type, language) %>%
        summarize(script_count = n(),
                  code = sum(code),
                  blank = sum(blank),
                  comment = sum(comment))




    ## info("=> Analyzing total corpus'\n")

    ## package_dirpaths <- c(.libPaths(), settings$input_package_code_dirpath)

    ## total_package_code_count <-
    ##     map_dfr(print(package_dirpaths),
    ##             function(package_dirpath) {
    ##                 relative_path <- "."

    ##                 relative_path_offset <- length(path_split(relative_path)[[1]])

    ##                 package_code_count <-
    ##                     count_lines_of_code(relative_path, c("C++", "C", "R"), FALSE,
    ##                                         package_dirpath) %>%
    ##                     mutate(language = if_else(language == "R", "R", "C/C++")) %>%
    ##                     mutate(path_components = path_split(filename)) %>%
    ##                     mutate(package_name = nth_element(1 + relative_path_offset)(path_components),
    ##                            script_type = nth_element(2 + relative_path_offset)(path_components),
    ##                            script_name = path_ext_remove(last_element(path_components))) %>%
    ##                     select(package_name, script_type, script_name, language, code, blank, comment) %>%
    ##                     filter(script_type %in% c("src", "R"))
    ##             }) %>%
    ## group_by(package_name, script_type, script_name) %>%
    ## filter(row_number() == 1) %>%
    ## ungroup() %>%
    ## print() %>%
    ## group_by(package_name, script_type, language) %>%
    ## summarize(script_count = n(),
    ##           code = sum(code),
    ##           blank = sum(blank),
    ##           comment = sum(comment))

    ## info("=> Analyzed total corpus'\n")
    ##mutate(script_type = sanitize_script_type(script_type)) %>%
    installed_corpus
}


summarize_corpus <- function(corpus) {
    summarized_corpus <-
        corpus %>%
        group_by(script_type, language) %>%
        summarize(script_count = sum(script_count),
                  package_count = length(unique(package_name)),
                  code = sum(code),
                  blank = sum(blank),
                  comment = sum(comment)) %>%
        ungroup()

    total_corpus <-
        summarized_corpus %>%
        group_by(language) %>%
        summarize_if(is.numeric, sum) %>%
        ungroup() %>%
        add_column(script_type = "Total", .before = 1)

    bind_rows(summarized_corpus,
              total_corpus)
}

analyze_traced_scripts <- function(settings) {

    info("=> Generating valid scripts\n")

    valid_scripts <-
        read_csv(settings$input_scanned_valid_script_dirpath, col_names = "filepath") %>%
        mutate(filepath = generate_script_filepath(settings$input_corpus_dirpath,
                                                   filepath))

    testthat_dirpaths <-
        valid_scripts %>%
        mutate(filepath = path_ext_remove(filepath)) %>%
        filter(path_file(filepath) == "testthat") %>%
        filter(dir_exists(path(settings$input_corpus_dirpath, filepath))) %>%
        mutate(filepath = str_c(filepath, "/"))

    valid_scripts <-
        bind_rows(valid_scripts, testthat_dirpaths)

    valid_scripts_filepath <-
        path(settings$corpus_data_dirpath, "valid_scripts", ext = "csv")

    write_csv(valid_scripts, valid_scripts_filepath, col_names = FALSE)

    info("=> Generated valid scripts in '", valid_scripts_filepath, "'\n")

    info("=> Counting program lines of code\n")

    traced_corpus <-
        count_lines_of_code(valid_scripts_filepath, "R",
                            TRUE, settings$input_corpus_dirpath) %>%
        mutate(path_components = path_split(filename)) %>%
        mutate(script_name = last_element(path_components),
               script_type = nth_element(2)(path_components),
               package_name = nth_element(1)(path_components),
               language = "R") %>%
        select(package_name, script_type, script_name, language, code, blank, comment)

    info("=> Counted program lines of code\n")

    traced_corpus
}


analyze_traced_packages <- function(settings, traced_scripts) {

    info("=> Generating package list\n")

    package_filepath <-
        path(settings$corpus_data_dirpath, "packages", ext = "csv")

    write_csv(tibble(package_name = program_package_names),
              package_filepath, col_names = FALSE)
}


analyze_traced_data <- function(settings, valid_packages) {

    compute_status <- function(begin, finish, error, noerror) {
        if_else(begin & finish & noerror, "SUCCESSFUL",
                if_else(begin & finish & error, "FAILED",
                        if_else(begin & !finish, "RUNNING",
                                if_else(!begin, "UNPROCESSED",
                                        "INVALID"))))
    }

    psum <- function(...) { pmap_dbl(list(...), sum) }

    traced_data <-
        path(settings$input_raw_data_dirpath, valid_packages) %>%
        dir_ls() %>%
        dir_ls() %>%
        dir_ls() %>%
        file_info() %>%
        select(filepath = path, size) %>%
        mutate(filepath_components = path_split(filepath)) %>%
        mutate(package_name = nth_element(-4)(filepath_components),
               script_type = nth_element(-3)(filepath_components),
               script_name = nth_element(-2)(filepath_components),
               filename = path_ext_remove(nth_element(-1)(filepath_components))) %>%
        select(-filepath, -filepath_components) %>%
        spread(filename, size) %>%
        select(-BEGIN, -FINISH, -ERROR, -NOERROR, -CONFIGURATION) %>%
        mutate(script_type = sanitize_script_type(script_type)) %>%
        mutate(Total = do.call(psum, select_if(., is.numeric)))

    traced_data
        #mutate(status = compute_status(!is.na(BEGIN), !is.na(FINISH),
        #                               !is.na(ERROR), !is.na(NOERROR))) %>%
}


summarize_traced_data <- function(traced_data) {
    summarized_traced_data <-
        traced_data %>%
        group_by(script_type) %>%
        summarize_if(is.numeric, sum) %>%
        ungroup()


    total_summarized_traced_data <-
        summarized_traced_data %>%
        summarize_if(is.numeric, sum) %>%
        add_column(script_type = "Total", .before = 1)


    bind_rows(summarized_traced_data,
              total_summarized_traced_data) %>%
        gather(filename, size, -script_type) %>%
        mutate(size = as.double(size)) %>%
        mutate(filename = path_ext_remove(filename)) %>%
        mutate(filename = str_to_title(str_replace(filename, "_", " "))) %>%
        as_tibble() %>%
        print(n = Inf)
}


analyze_corpus <- function(settings) {

    installed_corpus <- analyze_installed_corpus(settings)

    summarized_installed_corpus <- summarize_corpus(installed_corpus)

    traced_scripts <- analyze_traced_scripts(settings)

    summarized_traced_scripts <-
        traced_scripts %>%
        mutate(script_count = 1) %>%
        summarize_corpus()

    valid_packages <-
        traced_scripts %>%
        pull(package_name) %>%
        unique() %>%
        sort()

    traced_corpus <-
        installed_corpus %>%
        filter(package_name %in% valid_packages)

    summarized_traced_corpus <- summarize_corpus(traced_corpus)

    traced_data <- analyze_traced_data(settings, valid_packages)

    summarized_traced_data <- summarize_traced_data(traced_data)

    print(summarized_traced_data)

    return(list(installed_corpus = installed_corpus,
                summarized_installed_corpus = summarized_installed_corpus,
                traced_scripts = traced_scripts,
                summarized_traced_scripts = summarized_traced_scripts,
                traced_corpus = traced_corpus,
                summarized_traced_corpus = summarized_traced_corpus,
                traced_data = traced_data,
                summarized_traced_data = summarized_traced_data))

    ##                vignettes tests examples R src
    ## package count
    ## script count
    ## LOC

    ## installed
    ## traced


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
        path(settings$corpus_data_dirpath, "valid_scripts", ext = "csv")

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
        path(settings$corpus_data_dirpath, "packages", ext = "csv")

    write_csv(tibble(package_name = program_package_names),
              package_filepath, col_names = FALSE)

    info("=> Generated package list in '", package_filepath, "'\n")

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
        add_row(script_type = "Total", language = "R",
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
             write_data_table(df,
                              path(settings$corpus_data_dirpath, name),
                              binary = settings$binary,
                              compression_level = settings$compression_level)
         })
}

parse_program_arguments <- function() {

    usage <- "%prog input-corpus-dirpath input-raw-data-dirpath output-corpus-data-dirpath"

    description <- paste(
        "input-scanned-valid-script-filepath filepath for valid scripts",
        "input-corpus-dirpath                directory containing corpus files",
        "input-raw-data-dirpath              directory containing raw analysis data",
        "package-source-dirpath              directory containing package source",
        "package-test-dirpath                directory containing package tests",
        "package-example-dirpath             directory containing package examples",
        "package-vignette-dirpath            directory containing package vignettes",
        "corpus-data-dirpath                 directory in which the output files will be stored",
        sep = "\n")

    option_list <- list(
        make_option(c("--binary"),
                    action = "store_true",
                    default = FALSE,
                    help = "read data in binary format",
                    metavar = "binary"),

        make_option(c("--compression-level"),
                    action = "store",
                    type = "integer",
                    default = 0,
                    help = "compression level",
                    metavar = "compression_level")
    )

    option_parser <- OptionParser(usage = usage,
                                  description = description,
                                  add_help_option = TRUE,
                                  option_list = option_list)

    arguments <- parse_args2(option_parser)

    list(input_scanned_valid_script_dirpath = arguments$args[1],
         input_corpus_dirpath = arguments$args[2],
         input_raw_data_dirpath = arguments$args[3],
         package_src_dirpath = arguments$args[4],
         package_test_dirpath = arguments$args[5],
         package_example_dirpath = arguments$args[6],
         package_vignette_dirpath = arguments$args[7],
         corpus_data_dirpath = arguments$args[8],
         binary = arguments$options$binary,
         compression_level = arguments$options$compression_level)
}


main <- function() {
    settings <- parse_program_arguments()
    print(settings)
    analyses <- analyze_corpus(settings)
    serialize_analyses(analyses, settings)

    invisible(NULL)
}

main()
