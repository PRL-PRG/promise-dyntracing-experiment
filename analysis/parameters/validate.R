suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(promisedyntracer))
suppressPackageStartupMessages(library(styler))
suppressPackageStartupMessages(library(optparse))
suppressPackageStartupMessages(library(stringr))
suppressPackageStartupMessages(library(readr))
suppressPackageStartupMessages(library(fs))
suppressPackageStartupMessages(library(purrr))
suppressPackageStartupMessages(library(tibble))
suppressPackageStartupMessages(library(progress))

options(error = quote({dump.frames(to.file=FALSE); q();}))

## this ensures that all the columns of the tibble are printed
## irrespective of terminal width
options(tibble.width = Inf)

## TODO
## - nested promises created by the interpreter
## - wrapper vs non-wrapper functions
## - side effects

info <- function(...) cat((paste0(...)))

function_definition_serializer <- function(filepath, pb) {

    f <- file(filepath, "w")

    serialize_row <- function(function_id, function_name, definition, ...) {

        separator <- str_c(rep("#", 80), collapse = "")

        write_lines(separator, f)

        write_lines(str_c("## NAME  : ", function_name), f)

        write_lines(str_c("## ID    : ", function_id), f)

        other_arguments <- list(...)

        list(...) %>%
            imap(function(value, name) {
                write_lines(str_c("##", name, ":", value, sep = " "), f)
            })

        write_lines(separator, f)

        styled_definition <- definition

        tryCatch({
            styled_definition <-
                style_text(styled_definition,
                           reindention = tidyverse_reindention())
        },
        error = function(e) {
            print(e)
        },
        finally = {
            write_lines(styled_definition, f)
        })

        write_lines(separator, f)

        write_lines("\n\n", f)

        pb$tick()

        f
    }

    finish <- function() {
        close(f)
        filepath
    }

    list(serialize_row = serialize_row,
        finish = finish)
}


serialize_function_definitions <- function(data, dirpath, settings) {

    function_count = nrow(data)

    sequence_number <-
        c(1: (ceiling(function_count / settings$function_count))) %>%
        rep(each = settings$function_count) %>%
        `[`(1:function_count)

    data %>%
        add_column(sequence_number = sequence_number, .before = 1) %>%
        group_by(sequence_number) %>%
        do({
            filepath <- path(dirpath, str_c("group-", sequence_number[1]), ext = "R")

            pb <- progress_bar$new(format = "[:bar] :percent :eta",
                                   total = nrow(.data),
                                   clear = FALSE,
                                   width = 100)

            info("\n=> Writing to '", filepath, "'\n")

            serializer <- function_definition_serializer(filepath, pb)

            .data %>%
                select(-sequence_number) %>%
                pmap(serializer$serialize_row)

            serializer$finish()

            info("=> Written to '", filepath, "'\n")

            tibble(output_filepath = filepath)
        }) %>%
        ungroup()
}


multiparameter_functions <- function(analyses, settings) {

    multiparameter_function_dirpath <-
        path(settings$output_dirpath, "multiparameter_functions")

    dir_create(multiparameter_function_dirpath)

    analyses$closure_formal_parameter_count_table %>%
        filter(formal_parameter_count > 25 | formal_parameter_count == 0) %>%
        left_join(analyses$function_definitions, by = "function_id") %>%
        group_by(formal_parameter_count) %>%
        do({
            dirpath <- path(multiparameter_function_dirpath,
                            .data$formal_parameter_count[1])
            dir_create(dirpath)
            serialize_function_definitions(.data, dirpath, settings)
        }) %>%
    ungroup()
}


strictness <- function(analyses, settings) {

    strictness_dirpath <- path(settings$output_dirpath, "strictness")

    analyses$closure_strictness %>%
        left_join(analyses$function_definitions, by = c("function_id")) %>%
        group_by(strict, force_order_count) %>%
        do({
            dirpath <- path(strictness_dirpath,
                            c("non-strict", "strict")[.data$strict[1] + 1],
                            str_c("order-", .data$force_order_count[1]))
            dir_create(dirpath)

            serialize_function_definitions(.data, dirpath, settings)
        }) %>%
        ungroup()
}

metaprogramming <- function(analyses, settings) {

    metaprogram_dirpath <- path(settings$output_dirpath, "metaprogramming")

    analyses$closure_usage_class %>%
        left_join(analyses$function_definitions, by = "function_id") %>%
        filter(Metaprogram != "Never") %>%
        group_by(Metaprogram) %>%
        do({
            dirpath <- path(metaprogram_dirpath,
                            str_replace(.data$Metaprogram[1], "\\s\\&\\s", "-and-"))
            dir_create(dirpath)
            serialize_function_definitions(.data, dirpath, settings)
        }) %>%
    ungroup()
}


escaped_arguments <- function(analyses, settings) {

    escaped_argument_dirpath <- path(settings$output_dirpath, "escaped_arguments")

    escaped_argument_functions <-
        analyses$escaped_arguments %>%
        group_by(return_value_type, function_id) %>%
        summarize(formal_parameter_position = first(formal_parameter_position)) %>%
        ungroup() %>%
        left_join(analyses$function_definitions, by = "function_id") %>%
        group_by(return_value_type) %>%
        do({
            dirpath <- path(escaped_argument_dirpath, .data$return_value_type[1])
            dir_create(dirpath)
            serialize_function_definitions(.data, dirpath, settings)
        }) %>%
    ungroup()

}


non_local_returns <- function(analyses, settings) {

    non_local_dirpath <- path(settings$output_dirpath, "non_local_returns")

    dir_create(non_local_dirpath)

    analyses$promise_argument_returning_non_locally %>%
        left_join(analyses$function_definitions, by = "function_id") %>%
        serialize_function_definitions(non_local_dirpath, settings)
}


mutual_argument_forcing <- function(analyses, settings) {

    mutual_dirpath <- path(settings$output_dirpath, "mutual_argument_forcing")

    dir_create(mutual_dirpath)

    analyses$promise_argument_forced_by_promise_argument %>%
        left_join(analyses$function_definitions, by = "function_id") %>%
        serialize_function_definitions(mutual_dirpath, settings)
}



extract <- function(analyses, settings) {

    extractor <- eval(as.symbol(settings$analysis))

    extractor(analyses, settings)
}


scan_input_directory <- function(settings) {

    info("=> Scanning ", settings$input_dirpath, "\n")

    summarized_data_file_glob <-
        str_c("*",
              data_table_extension(settings$binary,
                                   settings$compression_level))

    summarized_data_filepaths <-
        settings$input_dirpath %>%
        dir_ls(recursive = FALSE, type = "file", glob = summarized_data_file_glob) %>%
        path_ext_remove() %>%
        path_ext_remove()

    analyses <- new.env(parent = emptyenv(), hash = TRUE)

    map(summarized_data_filepaths,
        function(summarized_data_filepath) {
            delayedAssign(path_file(summarized_data_filepath),
                          read_data_table(summarized_data_filepath,
                                          binary = settings$binary,
                                          compression_level = settings$compression_level),
                          assign.env = analyses)
        })

    info("=> Scanned ", settings$input_dirpath, "\n")

    analyses
}


parse_program_arguments <- function() {

    usage <- "%prog summarized-output-dirpath visualized-output-dirpath"
    description <- paste(
        "summarized-output-dirpath  directory containing summarized data files",
        "extracted-output-dirpath   directory to which function definitions will be extracted",
        "analysis                   analysis to run",
        "function-count             number of functions exported per file",
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

    list(input_dirpath = arguments$args[1],
         output_dirpath = arguments$args[2],
         analysis = arguments$args[3],
         function_count = as.integer(arguments$args[4]),
         binary = arguments$options$binary,
         compression_level = arguments$options$compression_level)
}


main <- function() {
    settings <- parse_program_arguments()

    dir_create(settings$output_dirpath)

    print(settings)

    analyses <- scan_input_directory(settings)

    extract(analyses, settings)
}


main()
