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
## - arguments forcing each other
## - non local returning arguments
## - nested promises created by the interpreter
## - multiple evaluation order
## - wrapper vs non-wrapper functions
## - side effects

info <- function(...) cat((paste0(...)))

function_definition_serializer <- function(filepath, pb) {

    f <- file(filepath, "w")

    serialize_row <- function(function_id, function_name, definition, script, ...) {

        separator <- str_c(rep("#", 80), collapse = "")

        write_lines(separator, f)

        write_lines(str_c("## NAME  : ", function_name), f)

        write_lines(str_c("## ID    : ", function_id), f)

        write_lines(str_c("## SOURCE: ", script), f)

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


serialize_closure_usage <- function(analyses, settings, data) {

    handle_group <- function(group, closure_class) {
        print(head(group))
        filepath <- path(settings$output_dirpath,
                         sprintf("%s-%06d",
                                 closure_class,
                                 group$sequence_number[1]),
                         ext = "txt")

        info("=> Writing to '", filepath, "'\n")

        serializer <- function_definition_serializer(filepath)

        group %>%
            select(-sequence_number) %>%
            pmap(serializer$serialize_row)

        serializer$finish()

        info("=> Written to '", filepath, "'\n\n")

        filepath
    }

    total_function_count <- nrow(data)

    sequence_number <-
        c(1: (ceiling(total_function_count / settings$function_count))) %>%
        rep(each = settings$function_count) %>%
        `[`(1:total_function_count)

    function_ids <- data$function_id

    analyses$function_definitions %>%
        filter(function_id %in% function_ids) %>%
        add_column(sequence_number = sequence_number, .before = 1) %>%
        group_by(sequence_number) %>%
        do(handle_group(.data, data$closure_class[1])) %>%
        ungroup()
}


many_formal_parameter_function_definitions <- function(analses, settings) {
    many_formal_parameter_function_definition_dirpath <-
        path(settings$output_dirpath,
             "many_formal_parameter_function_definitions")

    dir_create(many_formal_parameter_function_definition_dirpath)

    analyses$closure_formal_parameter_count_table %>%
        filter(formal_parameter_count > 25 | formal_parameter_count == 0) %>%
        left_join(analyses$function_definitions, by = "function_id") %>%
        select(function_id, function_name, definition, script, formal_parameter_count) %>%
        group_by(formal_parameter_count) %>%
        do({
            formal_parameter_count <- .data$formal_parameter_count[1]

            filepath <- path(many_formal_parameter_function_definition_dirpath,
                             formal_parameter_count,
                             ext = "R")

            pb <- progress_bar$new(format = "[:bar] :percent :eta",
                                   total = nrow(.data),
                                   clear = FALSE,
                                   width = 100)

            info("\n=> Writing to '", filepath, "'\n")

            serializer <- function_definition_serializer(filepath, pb)

            .data %>%
                pmap(serializer$serialize_row)

            serializer$finish()

            info("=> Written to '", filepath, "'\n")

            tibble(output_filepath = filepath)
        }) %>%
    ungroup()
}


functions <- function(analyses, settings) {

    #many_formal_parameter_function_definitions(analyses, settings)

    analyses$non_zero_parameter_multicall_closure_force_order_count %>%
        left_join(analyses$function_definitions) %>%
        select(function_id, function_name, definition, script, formal_parameter_count) %>%
        group_by(wrapper, compatible_force_order_counts)
}


escaped_arguments <- function(analyses, settings) {

    to_sequence <- function(params) {
        str_c("(",
              str_c(params, collapse = " "),
              ")",
              sep = "")
    }

    escaped_argument_functions <-
        analyses$escaped_arguments %>%
        group_by(return_value_type, function_id) %>%
        summarize(formal_parameter_position =
                      to_sequence(sort(unique(formal_parameter_position)))) %>%
        ungroup() %>%
        left_join(analyses$function_definitions, by = "function_id")

    escaped_argument_function_definition_dirpath <-
        path(settings$output_dirpath,
             "escaped_argument_function_definitions")

    dir_create(escaped_argument_function_definition_dirpath)

    escaped_argument_functions %>%
        group_by(return_value_type) %>%
        do({
            return_value_type <- .data$return_value_type[1]

            filepath <- path(escaped_argument_function_definition_dirpath,
                             str_c(str_to_lower(return_value_type)),
                             ext = "R")

            pb <- progress_bar$new(format = "[:bar] :percent :eta",
                                   total = nrow(.data),
                                   clear = FALSE,
                                   width = 100)

            info("\n=> Writing to '", filepath, "'\n")

            serializer <- function_definition_serializer(filepath, pb)

            .data %>%
                select(function_id, function_name, definition, script,
                       formal_parameter_position, return_value_type) %>%
                pmap(serializer$serialize_row)

            serializer$finish()

            info("=> Written to '", filepath, "'\n")

            tibble(output_filepath = filepath)
        }) %>%
    ungroup()

}

extract <- function(analyses, settings) {

    extractor <- eval(as.symbol(settings$analysis))

    extractor(analyses, settings)

    ## analyses$closure_usage_by_class_table %>%
    ##     filter(closure_use == "Either") %>%
    ##     group_by(closure_class) %>%
    ##     do(serialize_closure_usage(analyses, settings, .data)) %>%
    ##     ungroup()
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
