#!/usr/bin/env Rscript

## WARN - no good way in R to find path of current script
##        perhaps the only good solution is to create a
##        package for the analysis ?

##force_order_count should be changed to fore_order_call_count
##also add force_order_count field to the main function table.

options(error = quote({dump.frames(to.file=FALSE); q();}))

source("analysis/utils.R")
source("analysis/analysis.R")

reduce_analysis <- function(analyses) {
    parameters <-
        analyses$arguments %>%
        # filter(parameter_mode != "Unknown") %>%
        group_by(function_id, parameter_position, argument_mode, expression_type, value_type) %>%
        summarize(argument_count = n(),
                  "Force" = sum(as.logical(force_count)),
                  "Lookup" = sum(as.logical(lookup_count) & !as.logical(metaprogram_count)),
                  "Metaprogram" = sum(as.logical(metaprogram_count) & !as.logical(lookup_count)),
                  "Unused" = sum(!as.logical(metaprogram_count) & !as.logical(lookup_count)),
                  "Lookup & Metaprogram" = sum(as.logical(metaprogram_count) & as.logical(lookup_count))) %>%
        ungroup() %>%
        gather(argument_use_mode, argument_use_count, -function_id,
               -parameter_position, -argument_mode,
               -expression_type, -value_type, -argument_count)

    calls_with_missing_arguments <-
        analyses$arguments %>%
        group_by(call_id) %>%
        summarize(missing_argument = "Missing" %in% argument_mode) %>%
        ungroup()

    internal_call_id <-
        analyses$calls %>%
        filter(function_name == ".Internal") %>%
        pull(call_id)

    wrapper_id <-
        analyses$`call-graph` %>%
        group_by(caller_id) %>%
        summarize(calls = n(), callee_id = first(callee_id)) %>%
        ungroup() %>%
        filter(calls == 1 & callee_id %in% internal_call_id) %>%
        pull(caller_id)

    closures <-
        analyses$calls %>%
        filter(function_type == "Closure") %>%
        left_join(calls_with_missing_arguments, by = "call_id") %>%
        group_by(function_id, missing_argument, force_order) %>%
        summarize(force_order_call_count = n(),
                  function_type = first(function_type),
                  formal_parameter_count = first(formal_parameter_count),
                  function_name = list(unique(function_name)),
                  wrapper = all(call_id %in% wrapper_id)) %>%
        ungroup()

    print(list(parameters = parameters,
               closures = closures))
}

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

     closures <-
        analyses$closures %>%
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
               call_count = sum(force_order_call_count)) %>%
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
                  argument_count = sum(argument_count)) %>%
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
        mutate(function_class = ifelse(formal_parameter_count == 0, "Zero", function_class))

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

    print(list(closures = closures,
               once_called_functions = tibble(function_id = once_called_functions),
               parameters = parameters,
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
               function_count_by_wrapper_function_class_force_order_type_and_force_order_count = function_count_by_wrapper_function_class_force_order_type_and_force_order_count))

}

visualize_analyses <- function(analyses) {

    total_argument_count <- first(analyses$argument_count_by_argument_mode$total_argument_count)
    argument_count_by_argument_mode <-
        analyses$argument_count_by_argument_mode %>%
        ggplot(aes(x = argument_mode,
                   y = relative_argument_count)) +
        geom_col() +
        scale_y_continuous(sec.axis = sec_axis(~ . * total_argument_count,
                                               labels = count_labels),
                           labels = relative_labels) +
        labs(x = "Argument Mode",
             y = "Argument Count",
             title =  "Argument count distribution by argument mode") +
        scale_fill_gdocs() +
        theme(axis.text.x = element_text(angle = 60, hjust = 1))

    total_argument_count <- first(analyses$argument_count_by_argument_use_mode$total_argument_count)
    argument_count_by_argument_use_mode <-
        analyses$argument_count_by_argument_use_mode %>%
        ggplot(aes(x = argument_use_mode,
                   y = relative_argument_count)) +
        geom_col() +
        scale_y_continuous(sec.axis = sec_axis(~ . * total_argument_count,
                                               labels = count_labels),
                           labels = relative_labels) +
        labs(x = "Argument Use Mode",
             y = "Argument Count",
             title =  "Argument count distribution by argument use mode") +
        scale_fill_gdocs() +
        theme(axis.text.x = element_text(angle = 60, hjust = 1))

    total_argument_count <- first(analyses$argument_count_by_argument_mode_and_argument_use_mode$total_argument_count)

    argument_count_by_argument_mode_and_argument_use_mode <-
        analyses$argument_count_by_argument_mode_and_argument_use_mode %>%
        ggplot(aes(x = argument_mode,
                   y = relative_argument_count,
                   fill = argument_use_mode)) +
        geom_col() +
        scale_y_continuous(sec.axis = sec_axis(~ . * total_argument_count,
                                               labels = count_labels),
                           labels = relative_labels) +
        labs(x = "Argument Mode",
             y = "Argument Count",
             title =  "Argument count distribution by argument mode and argument use mode") +
        scale_fill_gdocs() +
        theme(axis.text.x = element_text(angle = 60, hjust = 1),
              legend.position = "bottom")

    total_argument_count <- first(analyses$argument_count_by_argument_use_mode_and_argument_mode$total_argument_count)
    argument_count_by_argument_use_mode_and_argument_mode <-
        analyses$argument_count_by_argument_use_mode_and_argument_mode %>%
        ggplot(aes(x = argument_use_mode,
                   y = relative_argument_count,
                   fill = argument_mode)) +
        geom_col() +
        scale_y_continuous(sec.axis = sec_axis(~ . * total_argument_count,
                                               labels = count_labels),
                           labels = relative_labels) +
        labs(x = "Argument Use Mode",
             y = "Argument Count",
             title =  "Argument count distribution by argument use mode and argument mode") +
        scale_fill_gdocs() +
        theme(axis.text.x = element_text(angle = 60, hjust = 1),
              legend.position = "bottom")

    total_parameter_count <- first(analyses$parameter_count_by_parameter_mode$total_parameter_count)
    parameter_count_by_parameter_mode <-
        analyses$parameter_count_by_parameter_mode %>%
        ggplot(aes(x = parameter_mode,
                   y = relative_parameter_count)) +
        geom_col() +
        scale_y_continuous(sec.axis = sec_axis(~ . * total_parameter_count,
                                               labels = count_labels),
                           labels = relative_labels) +
        labs(x = "Parameter Mode",
             y = "Parameter Count",
             title =  "Parameter count distribution by parameter mode") +
        scale_fill_gdocs() +
        theme(axis.text.x = element_text(angle = 60, hjust = 1))

    total_parameter_count <- first(analyses$parameter_count_by_parameter_use_mode$total_parameter_count)
    parameter_count_by_parameter_use_mode <-
        analyses$parameter_count_by_parameter_use_mode %>%
        ggplot(aes(x = parameter_use_mode,
                   y = relative_parameter_count)) +
        geom_col() +
        scale_y_continuous(sec.axis = sec_axis(~ . * total_parameter_count,
                                               labels = count_labels),
                           labels = relative_labels) +
        labs(x = "Parameter Use Mode",
             y = "Parameter Count",
             title =  "Parameter count distribution by parameter use mode") +
        scale_fill_gdocs() +
        theme(axis.text.x = element_text(angle = 60, hjust = 1))

    total_parameter_count <- first(analyses$parameter_count_by_parameter_class$total_parameter_count)
    parameter_count_by_parameter_class <-
        analyses$parameter_count_by_parameter_class %>%
        ggplot(aes(x = parameter_class,
                   y = relative_parameter_count)) +
        geom_col() +
        scale_y_continuous(sec.axis = sec_axis(~ . * total_parameter_count,
                                               labels = count_labels),
                           labels = relative_labels) +
        labs(x = "Parameter Class",
             y = "Parameter Count",
             title =  "Parameter count distribution by parameter class") +
        scale_fill_gdocs() +
        theme(axis.text.x = element_text(angle = 60, hjust = 1))

    parameter_count_by_parameter_mode_and_parameter_use_mode <-
        analyses$parameter_count_by_parameter_mode_and_parameter_use_mode %>%
        ggplot(aes(x = parameter_mode,
                   y = relative_parameter_count,
                   fill = parameter_use_mode)) +
        geom_col() +
        scale_y_continuous(sec.axis = sec_axis(~ . * total_parameter_count,
                                               labels = count_labels),
                           labels = relative_labels) +
        labs(x = "Parameter Mode",
             y = "Parameter Count",
             title =  "Parameter count distribution by parameter mode and parameter use mode") +
        scale_fill_gdocs() +
        theme(axis.text.x = element_text(angle = 60, hjust = 1),
              legend.position = "bottom")

    total_parameter_count <- first(analyses$parameter_count_by_parameter_use_mode_and_parameter_mode$total_parameter_count)
    parameter_count_by_parameter_use_mode_and_parameter_mode <-
        analyses$parameter_count_by_parameter_use_mode_and_parameter_mode %>%
        ggplot(aes(x = parameter_use_mode,
                   y = relative_parameter_count,
                   fill = parameter_mode)) +
        geom_col() +
        scale_y_continuous(sec.axis = sec_axis(~ . * total_parameter_count,
                                               labels = count_labels),
                           labels = relative_labels) +
        labs(x = "Parameter Use Mode",
             y = "Parameter Count",
             title =  "Parameter count distribution by parameter use mode and parameter mode") +
        scale_fill_gdocs() +
        theme(axis.text.x = element_text(angle = 60, hjust = 1),
              legend.position = "bottom")

    parameter_count_by_parameter_mode_and_parameter_class <-
        analyses$parameter_count_by_parameter_mode_and_parameter_class %>%
        ggplot(aes(x = parameter_mode,
                   y = relative_parameter_count,
                   fill = parameter_class)) +
        geom_col() +
        scale_y_continuous(sec.axis = sec_axis(~ . * total_parameter_count,
                                               labels = count_labels),
                           labels = relative_labels) +
        labs(x = "Parameter Mode",
             y = "Parameter Count",
             title =  "Parameter count distribution by parameter mode and parameter class") +
        scale_fill_gdocs() +
        theme(axis.text.x = element_text(angle = 60, hjust = 1),
              legend.position = "bottom")

    total_parameter_count <- first(analyses$parameter_count_by_parameter_class_and_parameter_mode$total_parameter_count)
    parameter_count_by_parameter_class_and_parameter_mode <-
        analyses$parameter_count_by_parameter_class_and_parameter_mode %>%
        ggplot(aes(x = parameter_class,
                   y = relative_parameter_count,
                   fill = parameter_mode)) +
        geom_col() +
        scale_y_continuous(sec.axis = sec_axis(~ . * total_parameter_count,
                                               labels = count_labels),
                           labels = relative_labels) +
        labs(x = "Parameter Class",
             y = "Parameter Count",
             title =  "Parameter count distribution by parameter class and parameter mode") +
        scale_fill_gdocs() +
        theme(axis.text.x = element_text(angle = 60, hjust = 1),
              legend.position = "bottom")

    parameter_count_by_parameter_use_mode_and_parameter_class <-
        analyses$parameter_count_by_parameter_use_mode_and_parameter_class %>%
        ggplot(aes(x = parameter_use_mode,
                   y = relative_parameter_count,
                   fill = parameter_class)) +
        geom_col() +
        scale_y_continuous(sec.axis = sec_axis(~ . * total_parameter_count,
                                               labels = count_labels),
                           labels = relative_labels) +
        labs(x = "Parameter Use Mode",
             y = "Parameter Count",
             title =  "Parameter count distribution by parameter use mode and parameter class") +
        scale_fill_gdocs() +
        theme(axis.text.x = element_text(angle = 60, hjust = 1),
              legend.position = "bottom")

    total_parameter_count <- first(analyses$parameter_count_by_parameter_class_and_parameter_use_mode$total_parameter_count)
    parameter_count_by_parameter_class_and_parameter_use_mode <-
        analyses$parameter_count_by_parameter_class_and_parameter_use_mode %>%
        ggplot(aes(x = parameter_class,
                   y = relative_parameter_count,
                   fill = parameter_use_mode)) +
        geom_col() +
        scale_y_continuous(sec.axis = sec_axis(~ . * total_parameter_count,
                                               labels = count_labels),
                           labels = relative_labels) +
        labs(x = "Parameter Class",
             y = "Parameter Count",
             title =  "Parameter count distribution by parameter class and parameter use mode") +
        scale_fill_gdocs() +
        theme(axis.text.x = element_text(angle = 60, hjust = 1),
              legend.position = "bottom")

    total_argument_count <- first(analyses$argument_count_by_parameter_class_and_argument_mode_and_argument_use_mode$total_argument_count)
    argument_count_by_parameter_class_and_argument_mode_and_argument_use_mode <-
        analyses$argument_count_by_parameter_class_and_argument_mode_and_argument_use_mode %>%
        ggplot(aes(x = argument_mode,
                   y = relative_argument_count,
                   fill = argument_use_mode)) +
        geom_col(position = "stack") +
        facet_wrap(~ parameter_class) +
        scale_y_continuous(sec.axis = sec_axis(~ . * total_argument_count,
                                               labels = count_labels),
                           labels = relative_labels) +
        labs(x = "Argument use mode",
             y = "Argument Count (%)",
             title =  "Argument count distribution by parameter class, argument mode and argument use mode") +
        scale_fill_gdocs() +
        theme(axis.text.x = element_text(angle = 60, hjust = 1),
              legend.position = "bottom")


    total_function_count <-
        first(analyses$function_count_by_call_count$total_function_count)

    print(total_function_count)

    function_count_by_call_count <-
        analyses$function_count_by_call_count %>%
        ggplot(aes(call_count, relative_function_count)) +
        geom_col() +
        scale_x_discrete(limits = c(1:10, "> 10")) +
        scale_y_continuous(sec.axis = sec_axis(~ . * total_function_count,
                                               labels = count_labels),
                           labels = relative_labels) +
        labs(x = "Call count",
             y = "Function count",
             title =  "Function count distribution by call count") +
        scale_fill_gdocs()

    function_count_by_formal_parameter_count <-
        analyses$function_count_by_formal_parameter_count %>%
        ggplot(aes(formal_parameter_count, relative_function_count)) +
        geom_col() +
        scale_x_discrete(limits = c(0:10, "> 10")) +
        scale_y_continuous(sec.axis = sec_axis(~ . * total_function_count,
                                               labels = count_labels),
                           labels = relative_labels) +
        labs(x = "Formal parameter count",
             y = "Function count",
             title =  "Function count distribution by formal parameter count") +
        scale_fill_gdocs()

    function_count_by_wrapper_force_order_type_and_force_order_count <-
        analyses$function_count_by_wrapper_force_order_type_and_force_order_count %>%
        ggplot(aes(x = force_order_count,
                   y = relative_function_count,
                   fill = force_order_type)) +
        geom_col(position = "dodge") +
        scale_x_discrete(limits = c(0:5, "> 5")) +
        facet_wrap(~ wrapper) +
        scale_y_continuous(sec.axis = sec_axis(~ . * total_function_count,
                                               labels = count_labels),
                           labels = relative_labels) +
        labs(x = "Force order count",
             y = "Function count",
             title =  "Function count by wrapper, force order type and force order count") +
        scale_fill_gdocs() +
        theme(axis.text.x = element_text(angle = 60, hjust = 1),
              legend.position = "bottom")

    wrapper_function_count_by_function_class_force_order_type_and_force_order_count <-
        analyses$function_count_by_wrapper_function_class_force_order_type_and_force_order_count %>%
        filter(wrapper == TRUE) %>%
        ggplot(aes(x = force_order_count,
                   y = relative_function_count,
                   fill = force_order_type)) +
        geom_col(position = "dodge") +
        scale_x_discrete(limits = c(0:5, "> 5")) +
        facet_wrap(~ function_class) +
        scale_y_continuous(sec.axis = sec_axis(~ . * total_function_count,
                                               labels = count_labels),
                           labels = relative_labels) +
        labs(x = "Force order count",
             y = "Function count",
             title = "Wrapper Function count by function class, force order type and force order count") +
        scale_fill_gdocs() +
        theme(axis.text.x = element_text(angle = 60, hjust = 1),
              legend.position = "bottom")


    nonwrapper_function_count_by_function_class_force_order_type_and_force_order_count <-
        analyses$function_count_by_wrapper_function_class_force_order_type_and_force_order_count %>%
        filter(wrapper == FALSE) %>%
        ggplot(aes(x = force_order_count,
                   y = relative_function_count,
                   fill = force_order_type)) +
        geom_col(position = "dodge") +
        scale_x_discrete(limits = c(0:5, "> 5")) +
        facet_wrap(~ function_class) +
        scale_y_continuous(sec.axis = sec_axis(~ . * total_function_count,
                                               labels = count_labels),
                           labels = relative_labels) +
        labs(x = "Force order count",
             y = "Function count",
             title =  "Nonwrapper Function count by function class, force order type and force order count") +
        scale_fill_gdocs() +
        theme(axis.text.x = element_text(angle = 60, hjust = 1),
              legend.position = "bottom")

    list(argument_count_by_argument_mode = argument_count_by_argument_mode,
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
         wrapper_function_count_by_function_class_force_order_type_and_force_order_count = wrapper_function_count_by_function_class_force_order_type_and_force_order_count,
         nonwrapper_function_count_by_function_class_force_order_type_and_force_order_count = nonwrapper_function_count_by_function_class_force_order_type_and_force_order_count)

}

latex_analyses <- function(analyses) {
    list()
}

main <- function() {
    analyzer <-
        create_analyzer("Parameter Analysis",
                        c("calls", "call-graph", "arguments"),
                        reduce_analysis,
                        combine_analyses,
                        summarize_analyses,
                        visualize_analyses,
                        latex_analyses)
    drive_analysis(analyzer)
}

main()
summary(warnings())

