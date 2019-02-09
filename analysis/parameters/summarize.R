#!/usr/bin/env Rscript

## WARN - no good way in R to find path of current script
##        perhaps the only good solution is to create a
##        package for the analysis ?

##force_order_count should be changed to fore_order_call_count
##also add force_order_count field to the main function table.

options(error = quote({dump.frames(to.file=FALSE); q();}))

## this ensures that all the columns of the tibble are printed
## irrespective of terminal width
options(tibble.width = Inf)

suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(optparse))
suppressPackageStartupMessages(library(magrittr))
suppressPackageStartupMessages(library(stringr))
suppressPackageStartupMessages(library(fs))
suppressPackageStartupMessages(library(promisedyntracer))
suppressPackageStartupMessages(library(purrr))
suppressPackageStartupMessages(library(tidyr))

info <- function(...) cat((paste0(...)))

summarize_analyses  <- function(analyses) {

    compute_parameter_mode <- function(argument_mode, argument_use_count) {
        tibble(argument_mode, argument_use_count) %>%
            filter(argument_use_count != 0) %>%
            pull(argument_mode) %>%
            unique() %>%
            sort() %>%
            str_c(collapse = " or ")
    }

    compute_parameter_use_mode <- function(argument_use_mode, argument_use_count) {

        parameter_use_mode <-
            tibble(argument_use_mode, argument_use_count) %>%
            filter(argument_use_count != 0 &
                   argument_use_mode != "Force") %>%
            pull(argument_use_mode)

        if ("Lookup & Metaprogram" %in% parameter_use_mode) {
            parameter_use_mode <- c("Lookup",
                                    "Metaprogram",
                                    setdiff(parameter_use_mode,
                                            "Lookup & Metaprogram"))
        }
        str_c(sort(unique(parameter_use_mode)), collapse = " or ")
    }

    compute_parameter_class <- function(argument_use_mode, argument_use_count) {

        group <- tibble(argument_use_mode, argument_use_count)

        unused_count <-
            group %>%
            filter(argument_use_mode == "Unused") %>%
            pull(argument_use_count) %>%
            sum()

        used_count <-
            group %>%
            filter(argument_use_mode != "Unused" &
                   argument_use_mode != "Forced") %>%
            pull(argument_use_count) %>%
            sum()

        if (unused_count == 0 & used_count != 0) "Always"
        else if(unused_count != 0 & used_count == 0) "Never"
        else if(unused_count != 0 & used_count != 0) "Sometimes"
        else "Indeterminate"
    }


    compute_function_class <- function(parameter_class) {
        str_c(sort(unique(parameter_class)), collapse = " & ")
    }

    compute_laziness_class <- function(formal_parameter_count, force_order) {
        force_order_count <- length(force_order)
        force_order_argument_count =
            map(str_split(force_order, " \\| "),
                function(o) {
                    r <- purrr::discard(o, function(v) v == "")
                    length(r)
                })
        laziness_class = if(all(force_order_count == 1))
                             "Non Lazy"
                         else if(all(formal_parameter_count == force_order_argument_count))
                             "Nontrivially Lazy"
                         else if(any(formal_parameter_count == force_order_argument_count))
                             "Maybe Nontrivially Lazy"
                         else "Trivially Lazy"
        laziness_class
    }

    closures <-
        analyses$closures %>%
        mutate(function_name = str_split(function_name, " \\| ")) %>%
        group_by(function_id, missing_argument, force_order) %>%
        summarize(function_type = first(function_type),
                  force_order_call_count = sum(force_order_call_count),
                  formal_parameter_count = first(formal_parameter_count),
                  wrapper = all(wrapper),
                  function_name = list(unique(unlist(function_name)))) %>%
        ungroup() %>%
        group_by(function_id) %>%
        mutate(function_name = str_c(unique(unlist(function_name)), collapse = " "),
               force_order_count = length(unique(force_order)),
               non_missing_force_order_count = length(unique(force_order[!missing_argument])),
               call_count = sum(force_order_call_count),
               laziness_class = compute_laziness_class(formal_parameter_count, force_order)) %>%
        ungroup()

    once_called_functions <-
        closures %>%
        filter(call_count == 1 & formal_parameter_count > 0) %>%
        pull(function_id) %>%
        unique()

    parameters <-
        analyses$parameters %>%
        group_by(function_id, parameter_position, argument_mode, expression_type, value_type, argument_use_mode) %>%
        summarize(argument_use_count = sum(argument_use_count),
                  argument_count = sum(argument_count),
                  escape = any(escape),
                  source = str_c(unique(str_c(package, script_type, script_name, sep = "::")), collapse=" | ")) %>%
        ungroup() %>%
        group_by(function_id, parameter_position) %>%
        mutate(parameter_mode = compute_parameter_mode(argument_mode,
                                                       argument_use_count),
               parameter_use_mode = compute_parameter_use_mode(argument_use_mode,
                                                               argument_use_count),
               parameter_class = compute_parameter_class(argument_use_mode,
                                                         argument_use_count)) %>%
        ungroup() %>%
        group_by(function_id) %>%
        mutate(function_class = compute_function_class(parameter_class)) %>%
        ungroup()

    parameters_multiply_called_functions <-
        parameters %>%
        filter(!(function_id %in% once_called_functions))

    function_class_table <-
        parameters %>%
        group_by(function_id) %>%
        summarize(function_class = first(function_class))

    closures <-
        closures %>%
        left_join(function_class_table, by = "function_id") %>%
        mutate(function_class = ifelse(formal_parameter_count == 0,
                                       "Zero",
                                       function_class))

    argument_execution_time_by_parameter_class <-
        analyses$argument_execution_time %>%
        left_join(select(parameters, function_id, parameter_position, parameter_class),
                  by = c("function_id", "parameter_position")) %>%
        select(parameter_class, expression_type, execution_time, argument_count) %>%
        mutate(execution_time = as.integer(execution_time / 1000000)) %>%
        group_by(parameter_class, expression_type, execution_time) %>%
        summarize(argument_count = sum(argument_count)) %>%
        ungroup() %>%
        group_by(parameter_class) %>%
        mutate(relative_argument_count = argument_count / sum(argument_count)) %>%
        ungroup()

    escaped_arguments <-
        parameters %>%
        filter(escape) %>%
        group_by(function_id, parameter_position) %>%
        summarize(source = first(source),
                  count = sum(argument_count)) %>%
        ungroup()

    argument_count_by_argument_mode <-
        parameters_multiply_called_functions %>%
        filter(argument_use_mode != "Force") %>%
        group_by(argument_mode) %>%
        summarize(argument_count = sum(argument_use_count)) %>%
        mutate(total_argument_count = sum(argument_count)) %>%
        mutate(relative_argument_count = argument_count / total_argument_count)

    argument_count_by_argument_use_mode <-
        parameters_multiply_called_functions %>%
        filter(argument_use_mode != "Force") %>%
        group_by(argument_use_mode) %>%
        summarize(argument_count = sum(argument_use_count)) %>%
        mutate(total_argument_count = sum(argument_count)) %>%
        mutate(relative_argument_count = argument_count / total_argument_count)

    argument_count_by_argument_mode_and_argument_use_mode <-
        parameters_multiply_called_functions %>%
        filter(argument_use_mode != "Force") %>%
        group_by(argument_mode, argument_use_mode) %>%
        summarize(argument_count = sum(argument_use_count)) %>%
        ungroup() %>%
        mutate(total_argument_count = sum(argument_count)) %>%
        mutate(relative_argument_count = argument_count / total_argument_count)

    argument_count_by_argument_use_mode_and_argument_mode <-
        parameters_multiply_called_functions %>%
        filter(argument_use_mode != "Force") %>%
        group_by(argument_use_mode, argument_mode) %>%
        summarize(argument_count = sum(argument_use_count)) %>%
        ungroup() %>%
        mutate(total_argument_count = sum(argument_count)) %>%
        mutate(relative_argument_count = argument_count / total_argument_count)

    parameter_count_by_parameter_mode <-
        parameters_multiply_called_functions %>%
        group_by(function_id, parameter_position, parameter_mode) %>%
        summarize(parameter_count = 1) %>%
        ungroup() %>%
        group_by(parameter_mode) %>%
        summarize(parameter_count = sum(parameter_count)) %>%
        ungroup() %>%
        mutate(total_parameter_count = sum(parameter_count)) %>%
        mutate(relative_parameter_count = parameter_count / total_parameter_count)

    parameter_count_by_parameter_use_mode <-
        parameters_multiply_called_functions %>%
        group_by(function_id, parameter_position, parameter_use_mode) %>%
        summarize(parameter_count = 1) %>%
        group_by(parameter_use_mode) %>%
        summarize(parameter_count = sum(parameter_count)) %>%
        mutate(total_parameter_count = sum(parameter_count)) %>%
        mutate(relative_parameter_count = parameter_count / total_parameter_count)

    parameter_count_by_parameter_class <-
        parameters_multiply_called_functions %>%
        group_by(function_id, parameter_position, parameter_class) %>%
        summarize(parameter_count = 1) %>%
        group_by(parameter_class) %>%
        summarize(parameter_count = sum(parameter_count)) %>%
        mutate(total_parameter_count = sum(parameter_count)) %>%
        mutate(relative_parameter_count = parameter_count / total_parameter_count)

    parameter_count_by_parameter_mode_and_parameter_use_mode <-
        parameters_multiply_called_functions %>%
        group_by(function_id, parameter_position, parameter_mode, parameter_use_mode) %>%
        summarize(parameter_count = 1) %>%
        group_by(parameter_mode, parameter_use_mode) %>%
        summarize(parameter_count = sum(parameter_count)) %>%
        ungroup() %>%
        mutate(total_parameter_count = sum(parameter_count)) %>%
        mutate(relative_parameter_count = parameter_count / total_parameter_count)

    parameter_count_by_parameter_use_mode_and_parameter_mode <-
        parameters_multiply_called_functions %>%
        group_by(function_id, parameter_position, parameter_mode, parameter_use_mode) %>%
        summarize(parameter_count = 1) %>%
        group_by(parameter_use_mode, parameter_mode) %>%
        summarize(parameter_count = sum(parameter_count)) %>%
        ungroup() %>%
        mutate(total_parameter_count = sum(parameter_count)) %>%
        mutate(relative_parameter_count = parameter_count / total_parameter_count)

    parameter_count_by_parameter_mode_and_parameter_class <-
        parameters_multiply_called_functions %>%
        group_by(function_id, parameter_position, parameter_mode, parameter_class) %>%
        summarize(parameter_count = 1) %>%
        group_by(parameter_mode, parameter_class) %>%
        summarize(parameter_count = sum(parameter_count)) %>%
        ungroup() %>%
        mutate(total_parameter_count = sum(parameter_count)) %>%
        mutate(relative_parameter_count = parameter_count / total_parameter_count)

    parameter_count_by_parameter_class_and_parameter_mode <-
        parameters_multiply_called_functions %>%
        group_by(function_id, parameter_position, parameter_mode, parameter_class) %>%
        summarize(parameter_count = 1) %>%
        group_by(parameter_class, parameter_mode) %>%
        summarize(parameter_count = sum(parameter_count)) %>%
        ungroup() %>%
        mutate(total_parameter_count = sum(parameter_count)) %>%
        mutate(relative_parameter_count = parameter_count / total_parameter_count)

    parameter_count_by_parameter_use_mode_and_parameter_class <-
        parameters_multiply_called_functions %>%
        group_by(function_id, parameter_position, parameter_use_mode, parameter_class) %>%
        summarize(parameter_count = 1) %>%
        group_by(parameter_use_mode, parameter_class) %>%
        summarize(parameter_count = sum(parameter_count)) %>%
        ungroup() %>%
        mutate(total_parameter_count = sum(parameter_count)) %>%
        mutate(relative_parameter_count = parameter_count / total_parameter_count)

    parameter_count_by_parameter_class_and_parameter_use_mode <-
        parameters_multiply_called_functions %>%
        group_by(function_id, parameter_position, parameter_use_mode, parameter_class) %>%
        summarize(parameter_count = 1) %>%
        group_by(parameter_class, parameter_use_mode) %>%
        summarize(parameter_count = sum(parameter_count)) %>%
        ungroup() %>%
        mutate(total_parameter_count = sum(parameter_count)) %>%
        mutate(relative_parameter_count = parameter_count / total_parameter_count)

    argument_count_by_parameter_class_and_argument_mode_and_argument_use_mode <-
        parameters_multiply_called_functions %>%
        filter(argument_use_mode != "Force") %>%
        group_by(parameter_class, argument_mode, argument_use_mode) %>%
        summarize(argument_count = sum(argument_use_count)) %>%
        ungroup() %>%
        mutate(total_argument_count = sum(argument_count)) %>%
        mutate(relative_argument_count = argument_count / total_argument_count)


    total_function_count <- length(unique(closures$function_id))

    function_count_by_call_count <-
        closures %>%
        group_by(call_count) %>%
        summarize(function_count = length(unique(function_id))) %>%
        ungroup()

    frequently_called_function_count <-
        function_count_by_call_count %>%
        filter(call_count > 10) %>%
        pull(function_count) %>%
        sum()

    function_count_by_call_count <-
        function_count_by_call_count %>%
        mutate(call_count = as.character(call_count)) %>%
        add_row(call_count = "> 10", function_count = frequently_called_function_count) %>%
        mutate(total_function_count = total_function_count) %>%
        mutate(relative_function_count = function_count / total_function_count)

    function_count_by_formal_parameter_count <-
        closures %>%
        group_by(formal_parameter_count) %>%
        summarize(function_count = length(unique(function_id))) %>%
        ungroup()

    many_argument_function_count <-
        function_count_by_formal_parameter_count %>%
        filter(formal_parameter_count > 10) %>%
        pull(function_count) %>%
        sum()

    function_count_by_formal_parameter_count <-
        function_count_by_formal_parameter_count %>%
        mutate(formal_parameter_count = as.character(formal_parameter_count)) %>%
        add_row(formal_parameter_count = "> 10", function_count = many_argument_function_count) %>%
        mutate(total_function_count = total_function_count) %>%
        mutate(relative_function_count = function_count / total_function_count)

    function_count_by_wrapper_function_class_force_order_type_and_force_order_count <-
        closures %>%
        group_by(function_id) %>%
        summarize("Available or Missing" = first(force_order_count),
                  "Available" = first(non_missing_force_order_count),
                  formal_parameter_count = first(formal_parameter_count),
                  wrapper = first(wrapper),
                  function_class = first(function_class)) %>%
        ungroup() %>%
        gather(force_order_type, force_order_count, -function_id, -function_class, -formal_parameter_count, -wrapper) %>%
        group_by(wrapper, function_class, force_order_type, force_order_count) %>%
        summarize(function_count = n()) %>%
        ungroup()

    function_count_by_wrapper_force_order_type_and_force_order_count <-
        function_count_by_wrapper_function_class_force_order_type_and_force_order_count %>%
        group_by(wrapper, force_order_type, force_order_count) %>%
        summarize(function_count = sum(function_count)) %>%
        ungroup()

    many_force_order_count_function_count <-
        function_count_by_wrapper_force_order_type_and_force_order_count %>%
        filter(force_order_count > 5) %>%
        group_by(wrapper, force_order_type) %>%
        summarize(function_count = sum(function_count)) %>%
        ungroup() %>%
        mutate(force_order_count = "> 5")

    function_count_by_wrapper_force_order_type_and_force_order_count <-
        function_count_by_wrapper_force_order_type_and_force_order_count %>%
        mutate(force_order_count = as.character(force_order_count)) %>%
        rbind(many_force_order_count_function_count) %>%
        mutate(total_function_count = total_function_count) %>%
        mutate(relative_function_count = function_count / total_function_count)

    many_force_order_count_function_count <-
        function_count_by_wrapper_function_class_force_order_type_and_force_order_count %>%
        filter(force_order_count > 5) %>%
        group_by(wrapper, function_class, force_order_type, force_order_count) %>%
        summarize(function_count = sum(function_count)) %>%
        ungroup() %>%
        mutate(force_order_count = "> 5")

    function_count_by_wrapper_function_class_force_order_type_and_force_order_count <-
        function_count_by_wrapper_function_class_force_order_type_and_force_order_count %>%
        mutate(force_order_count = as.character(force_order_count)) %>%
        rbind(many_force_order_count_function_count) %>%
        mutate(total_function_count = total_function_count) %>%
        mutate(relative_function_count = function_count / total_function_count)

    total_argument_count <-
        analyses$argument_call_depth %>%
        pull(argument_count) %>%
        sum()

    argument_count_by_call_depth_and_expression_type <-
        analyses$argument_call_depth %>%
        group_by(call_depth, expression_type) %>%
        summarize(argument_count = sum(argument_count)) %>%
        ungroup()

    many_argument_call_depth <-
        argument_count_by_call_depth_and_expression_type %>%
        filter(call_depth > 10) %>%
        mutate(call_depth = "> 10") %>%
        group_by(call_depth, expression_type) %>%
        summarize(argument_count = sum(argument_count)) %>%
        ungroup()

    argument_count_by_call_depth_and_expression_type <-
        argument_count_by_call_depth_and_expression_type %>%
        mutate(call_depth = as.character(call_depth)) %>%
        rbind(many_argument_call_depth) %>%
        mutate(relative_argument_count = argument_count / total_argument_count)


    argument_count_by_promise_depth_and_expression_type <-
        analyses$argument_promise_depth %>%
        group_by(promise_depth, expression_type) %>%
        summarize(argument_count = sum(argument_count)) %>%
        ungroup()

    many_argument_promise_depth <-
        argument_count_by_promise_depth_and_expression_type %>%
        filter(promise_depth > 10) %>%
        mutate(promise_depth = "> 10") %>%
        group_by(promise_depth, expression_type) %>%
        summarize(argument_count = sum(argument_count)) %>%
        ungroup()

    argument_count_by_promise_depth_and_expression_type <-
        argument_count_by_promise_depth_and_expression_type %>%
        mutate(promise_depth = as.character(promise_depth)) %>%
        rbind(many_argument_promise_depth) %>%
        mutate(relative_argument_count = argument_count / total_argument_count)


    argument_count_by_nested_promise_depth_and_expression_type <-
        analyses$argument_nested_promise_depth %>%
        group_by(nested_promise_depth, expression_type) %>%
        summarize(argument_count = sum(argument_count)) %>%
        ungroup()

    many_argument_nested_promise_depth <-
        argument_count_by_nested_promise_depth_and_expression_type %>%
        filter(nested_promise_depth > 10) %>%
        mutate(nested_promise_depth = "> 10") %>%
        group_by(nested_promise_depth, expression_type) %>%
        summarize(argument_count = sum(argument_count)) %>%
        ungroup()

    argument_count_by_nested_promise_depth_and_expression_type <-
        argument_count_by_nested_promise_depth_and_expression_type %>%
        mutate(nested_promise_depth = as.character(nested_promise_depth)) %>%
        rbind(many_argument_nested_promise_depth) %>%
        mutate(relative_argument_count = argument_count / total_argument_count)

    list(closures = closures,
         once_called_functions = tibble(function_id = once_called_functions),
         parameters = parameters,
         argument_execution_time_by_parameter_class = argument_execution_time_by_parameter_class,
         escaped_arguments = escaped_arguments,
         parameters_multiply_called_functions = parameters_multiply_called_functions,
         argument_count_by_argument_mode = argument_count_by_argument_mode,
         argument_count_by_argument_use_mode = argument_count_by_argument_use_mode,
         argument_count_by_argument_mode_and_argument_use_mode = argument_count_by_argument_mode_and_argument_use_mode,
         argument_count_by_argument_use_mode_and_argument_mode = argument_count_by_argument_use_mode_and_argument_mode,
         parameter_count_by_parameter_mode = parameter_count_by_parameter_mode,
         parameter_count_by_parameter_use_mode = parameter_count_by_parameter_use_mode,
         parameter_count_by_parameter_class = parameter_count_by_parameter_class,
         parameter_count_by_parameter_mode_and_parameter_use_mode = parameter_count_by_parameter_mode_and_parameter_use_mode,
         parameter_count_by_parameter_use_mode_and_parameter_mode = parameter_count_by_parameter_use_mode_and_parameter_mode,
         parameter_count_by_parameter_mode_and_parameter_class = parameter_count_by_parameter_mode_and_parameter_class,
         parameter_count_by_parameter_class_and_parameter_mode = parameter_count_by_parameter_class_and_parameter_mode,
         parameter_count_by_parameter_use_mode_and_parameter_class = parameter_count_by_parameter_use_mode_and_parameter_class,
         parameter_count_by_parameter_class_and_parameter_use_mode = parameter_count_by_parameter_class_and_parameter_use_mode,
         argument_count_by_parameter_class_and_argument_mode_and_argument_use_mode = argument_count_by_parameter_class_and_argument_mode_and_argument_use_mode,
         function_count_by_call_count = function_count_by_call_count,
         function_count_by_formal_parameter_count = function_count_by_formal_parameter_count,
         function_count_by_wrapper_force_order_type_and_force_order_count = function_count_by_wrapper_force_order_type_and_force_order_count,
         function_count_by_wrapper_function_class_force_order_type_and_force_order_count = function_count_by_wrapper_function_class_force_order_type_and_force_order_count,
         argument_count_by_call_depth_and_expression_type = argument_count_by_call_depth_and_expression_type,
         argument_count_by_promise_depth_and_expression_type = argument_count_by_promise_depth_and_expression_type,
         argument_count_by_nested_promise_depth_and_expression_type = argument_count_by_nested_promise_depth_and_expression_type)
}


summarize_combined_data <- function(settings, combined_data_table) {

    info("=> Starting summarization\n")

    combined_data_filepaths <- combined_data_table$filepath

    summarized_data_filepaths <-
        combined_data_filepaths %>%
        map(promisedyntracer::read_data_table) %>%
        set_names(path_ext_remove(path_file(combined_data_filepaths))) %>%
        summarize_analyses() %>%
        imap(
            function(df, name) {
                output_filepath <- path(settings$output_dirpath, name)

                promisedyntracer::write_data_table(df, output_filepath,
                                                   compression_level = 0,
                                                   binary = FALSE)
                path(output_filepath, ext = "csv")
            }
        )

    info("=> Finished summarization\n")

    tibble(filepath = unlist(summarized_data_filepaths))
}


scan_input_dirpath <- function(settings) {

    info("=> Scanning for combined data files in ", settings$input_dirpath, "\n")

    combined_data_table <-
        settings$input_dirpath %>%
        dir_ls(type = "file") %>%
        {tibble(filepath = .)}

    info("=> Found ", nrow(combined_data_table), " data files\n")

    combined_data_table
}


parse_program_arguments <- function() {

    usage <- "%prog combined-output-dirpath summarized-output-dirpath"
    description <- paste(
        "combined-output-dirpath    directory containing combined data files",
        "summarized-output-dirpath  directory to which summarized data will be exported",
        sep = "\n")

    option_parser <- OptionParser(usage = usage,
                                  description = description,
                                  add_help_option = TRUE,
                                  option_list = list())

    arguments <- parse_args2(option_parser)

    list(input_dirpath = arguments$args[1],
         output_dirpath = arguments$args[2])

}


main <- function() {
    settings <- parse_program_arguments()
    dir_create(settings$output_dirpath)
    print(settings)
    combined_data_table <- scan_input_dirpath(settings)
    print(combined_data_table)
    print(summarize_combined_data(settings, combined_data_table), n = Inf)
}


main()
