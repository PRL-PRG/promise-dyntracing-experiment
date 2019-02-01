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
suppressPackageStartupMessages(library(ggplot2))
suppressPackageStartupMessages(library(ggthemes))
suppressPackageStartupMessages(library(scales))

source("analysis/utils.R")

info <- function(...) cat((paste0(...)))

visualize_analyses <- function(analyses) {

    argument_execution_time_by_parameter_class <-
        analyses$argument_execution_time_by_parameter_class %>%
        ggplot(aes(parameter_class, execution_time,
                   color = parameter_class,
                   weight = relative_argument_count)) +
        geom_violin() +
        scale_y_log10() +
        labs(x = "Parameter Class",
             y = "Execution Time (ms)",
             title =  "Argument count distribution by execution time and parameter_class") +
        scale_fill_gdocs() +
        theme(axis.text.x = element_text(angle = 60, hjust = 1))

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

    list(argument_execution_time_by_parameter_class = argument_execution_time_by_parameter_class,
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
         wrapper_function_count_by_function_class_force_order_type_and_force_order_count = wrapper_function_count_by_function_class_force_order_type_and_force_order_count,
         nonwrapper_function_count_by_function_class_force_order_type_and_force_order_count = nonwrapper_function_count_by_function_class_force_order_type_and_force_order_count)

}

visualize_summarized_data <- function(settings, summarized_data_table) {

    info("=> Starting visualization\n")

    summarized_data_filepaths <- summarized_data_table$filepath

    visualized_data_filepaths <-
        summarized_data_filepaths %>%
        map(promisedyntracer::read_data_table) %>%
        set_names(path_ext_remove(path_file(summarized_data_filepaths))) %>%
        visualize_analyses() %>%
        imap(
            function(visualization, name) {
                output_filepath <- path(settings$output_dirpath,
                                        name,
                                        ext = "png")
                ggsave(output_filepath, visualization)
                output_filepath
            }
        )

    info("=> Finished visualization\n")

    tibble(filepath = unlist(visualized_data_filepaths))
}


scan_input_dirpath <- function(settings) {

    info("=> Scanning for summarized data files in ", settings$input_dirpath, "\n")

    summarized_data_table <-
        settings$input_dirpath %>%
        dir_ls(type = "file") %>%
        {tibble(filepath = .)}

    info("=> Found ", nrow(summarized_data_table), " data files\n")

    summarized_data_table
}


parse_program_arguments <- function() {

    usage <- "%prog summarized-output-dirpath visualized-output-dirpath"
    description <- paste(
        "summarized-output-dirpath  directory containing summarized data files",
        "visualized-output-dirpath  directory to which visualizations will be exported",
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
    summarized_data_table <- scan_input_dirpath(settings)
    print(summarized_data_table)

    visualize_summarized_data(settings, summarized_data_table) %>%
        print(n = Inf)
}


main()
