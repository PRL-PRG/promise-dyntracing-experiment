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
    analyses$`parameter-position-type` <-
        analyses$`parameter-usage-count` %>%
        # filter(parameter_mode != "Unknown") %>%
        group_by(function_id, parameter_position, argument_mode) %>%
        summarize(argument_count = n(),
                  parameter_use_count = sum(as.logical(lookup_count + metaprogram_count)),
                  lookup_count = sum(as.logical(lookup_count) & !as.logical(metaprogram_count)),
                  metaprogram_count = sum(as.logical(metaprogram_count) & !as.logical(lookup_count)),
                  unused_count = sum(!as.logical(metaprogram_count) & !as.logical(lookup_count)),
                  lookup_and_metaprogram_count = sum(as.logical(metaprogram_count) & as.logical(lookup_count))) %>%
        ungroup()

    ## analyses$`parameter`
    ## group_by(function_id, call_id) %>%
    ##     summarize(argument_mode = str_c(argument_mode, collapse=" + "),
    ##               expression_type = str_c(expression_type, collapse=" + "),
    ##               value_type = str_c(value_type, collapse=" + "),
    ##               force_order = parameter_position[force_index]
    analyses
}

classify_argument_class <- function(argument_use_count) {
    ifelse(argument_use_count == 0, "Never", "Always")
}

classify_parameter_position <- function(use_count, calls) {
    ifelse(use_count == 0, "Never",
    ifelse(use_count == calls, "Always",
           "Sometimes"))
    ## else if() "Always"
    ## else "Sometimes"
}

compute_parameter_mode <- function(parameter_mode) {

    default <- sum(parameter_mode == "Default")
    custom <- sum(parameter_mode == "Custom")
    nonpromise <- sum(parameter_mode == "Nonpromise")
    missing_value <- sum(parameter_mode == "Missing")

    mode <- c()

    if(default != 0) mode <- c("Default")
    if(custom != 0) mode <- c(mode, "Custom")
    if(nonpromise != 0) mode <- c(mode, "Nonpromise")
    if(missing_value != 0) mode <- c(mode, "Missing")

    str_c(mode, collapse = " + ")
}

classify_function <- function(position_type) {

    never <- sum(position_type == "Never")
    always <- sum(position_type == "Always")
    sometimes <- sum(position_type == "Sometimes")

    classification <- c()

    if (always != 0) classification <- c("Always")
    if (sometimes != 0) classification <- c(classification, "Sometimes")
    if (never != 0) classification <- c(classification, "Never")

    str_c(classification, collapse = " + ")

    ## if (sometimes == 0 & never == 0) "Always"
    ## else if (always == 0 & never == 0) "Sometimes"
    ## else if (always == 0 & sometimes == 0) "Never"
    ## else if (never == 0) "Always + Sometimes"
    ## else if (always == 0) "Never + Sometimes"
    ## else if (sometimes == 0) "Never + Always"
    ## else "Impossible"
}

classify_argument_use_mode <- function(argument_use_type) {
    ifelse(argument_use_type == 0, "Unused",
    ifelse(argument_use_type == 1, "Lookup",
    ifelse(argument_use_type == 2, "Metaprogram",
    ifelse(argument_use_type == 3, "Lookup + Metaprogram",
           "Argument Use Mode Error"))))
}

compute_closure_call_count_summary_distribution  <- function(closures) {

    closure_call_count_summary_distribution  <-
        closures %>%
        group_by(call_count) %>%
        summarize(function_count = n())

    most_called_closure_count <- sum(filter(closure_call_count_summary_distribution,
                                            call_count > 10)$function_count)

    closure_call_count_summary_distribution <-
        closure_call_count_summary_distribution %>%
        mutate(call_count = as.character(call_count)) %>%
        add_row(call_count = "> 10",
                function_count = most_called_closure_count) %>%
        mutate(total_function_count = length(unique(closures$function_id))) %>%
        mutate(relative_function_count = function_count / total_function_count)

    closure_call_count_summary_distribution
}

compute_closure_argument_count_summary_distribution <- function(closures) {

    closure_argument_count_summary_distribution <-
        closures %>%
        group_by(parameter_count) %>%
        summarize(function_count = n())

    most_arguments_closure_count <- sum(filter(closure_argument_count_summary_distribution,
                                               parameter_count > 10)$function_count)

    closure_argument_count_summary_distribution  <-
        closure_argument_count_summary_distribution %>%
        mutate(parameter_count = as.character(parameter_count)) %>%
        add_row(parameter_count = "> 10",
                function_count = most_arguments_closure_count) %>%
        mutate(total_function_count = length(unique(closures$function_id))) %>%
        mutate(relative_function_count = function_count / total_function_count)

    closure_argument_count_summary_distribution
}

compute_closure_force_order_distribution_summary <- function(closure_force_order_distribution,
                                                             total_function_count) {

    closure_force_order_distribution_summary <-
        closure_force_order_distribution %>%
        group_by(force_order_count) %>%
        summarize(function_count = length(unique(function_id)))

    most_closure_force_order_count <- sum(filter(closure_force_order_distribution_summary,
                                                 force_order_count > 10)$function_count)

    closure_force_order_distribution_summary  <-
        closure_force_order_distribution_summary %>%
        mutate(force_order_count = as.character(force_order_count)) %>%
        add_row(force_order_count = "> 10",
                function_count = most_closure_force_order_count) %>%
        mutate(total_function_count = total_function_count) %>%
        mutate(relative_function_count = function_count / total_function_count)

    closure_force_order_distribution_summary
}

summarize_analyses  <- function(analyses) {

    closures <-
        analyses$functions %>%
        filter(function_type == "Closure") %>%
        group_by(function_id) %>%
        summarize(call_count = sum(as.numeric(call_count)),
                  parameter_count = first(parameter_count),
                  function_name = str_c("`", unique(function_name), "`", collapse = " ")) %>%
        ungroup()

    total_function_count <- length(unique(closures$function_id))

    total_call_count <- sum(closures$call_count)

    zero_argument_closure_count <- nrow(filter(closures, parameter_count == 0))

    zero_argument_closure_call_count <- sum(filter(closures, parameter_count == 0)$call_count)

    called_once_closure_count <- nrow(filter(closures, parameter_count != 0 & call_count == 1))

    called_once_closure_call_count <- sum(filter(closures, parameter_count != 0 & call_count == 1)$call_count)

    total_parameter_count = sum(closures$call_count * closures$parameter_count)

    total_argument_count = sum(analyses$`parameter-position-type`$argument_count)

    closure_call_count_summary_distribution <-
        compute_closure_call_count_summary_distribution(closures)

    closure_argument_count_summary_distribution <-
        compute_closure_argument_count_summary_distribution(closures)

    closure_force_order_distribution <-
        analyses$`parameter-force-order` %>%
        group_by(function_id, force_order) %>%
        summarize(force_order_call_count = sum(force_order_count)) %>%
        mutate(force_order_count = length(unique(force_order))) %>%
        print()

    closure_force_order_distribution <-
        closure_force_order_distribution %>%
        left_join(closures) %>%
        select(function_id, function_name,
               force_order, call_count,
               parameter_count, force_order_count, force_order_call_count)

    closure_force_order_distribution_summary <-
        compute_closure_force_order_distribution_summary(closure_force_order_distribution,
                                                         total_function_count)

    classification_summary  <-
        analyses$`parameter-position-type` %>%
        group_by(function_id, parameter_position) %>%
        summarize(argument_count = sum(argument_count),
                  parameter_use_count = sum(parameter_use_count),
                  lookup_count = sum(lookup_count),
                  metaprogram_count = sum(metaprogram_count),
                  unused_count = sum(unused_count),
                  lookup_and_metaprogram_count = sum(lookup_and_metaprogram_count),
                  ## lookup_count = sum(as.logical(lookup_count)),
                  ##                  metaprogram_count = sum(as.logical(metaprogram_count)),
                  ##                  argument_mode = parameter_mode,
                  parameter_mode = compute_parameter_mode(argument_mode)) %>%
        ungroup() %>%
        left_join(closures) %>%
        filter(call_count > 1 | parameter_count > 0) %>%
        mutate(parameter_class = classify_parameter_position(parameter_use_count, call_count)) %>%
        group_by(function_id) %>%
        mutate(function_class = classify_function(parameter_class)) %>%
        ungroup() %>%
        select(function_id, function_name, parameter_position,
               argument_count,
               parameter_use_count, lookup_count, metaprogram_count,
               unused_count, lookup_and_metaprogram_count, call_count,
               parameter_class, parameter_mode, function_class)

    total_parameter_count <- nrow(classification_summary)

    argument_classification_summary  <-
        analyses$`parameter-position-type` %>%
        group_by(function_id, parameter_position, argument_mode) %>%
        summarize(argument_count = sum(argument_count),
                  parameter_use_count = sum(parameter_use_count),
                  lookup_count = sum(lookup_count),
                  metaprogram_count = sum(metaprogram_count),
                  unused_count = sum(unused_count),
                  lookup_and_metaprogram_count = sum(lookup_and_metaprogram_count)) %>%
        ungroup() %>%
        left_join(closures) %>%
        filter(call_count > 1 && parameter_count > 0) %>%
        mutate(parameter_class = classify_parameter_position(parameter_use_count,
                                                             call_count)) %>%
        select(function_id, function_name, parameter_position,
               argument_count, parameter_use_count, lookup_count, metaprogram_count,
               unused_count, lookup_and_metaprogram_count, call_count,
               parameter_class, argument_mode)

#     classification_summary  <-
#         analyses$`parameter-position-type` %>%
#         group_by(function_id, parameter_position) %>%
#         mutate(argument_use_mode =
#                    classify_argument_use_mode(
#                        bitwOr(bitwShiftL(as.logical(metaprogram_count),
#                                          as.logical(lookup_count)))),
#                parameter_mode = compute_parameter_mode(parameter_mode)) %>%
#         ungroup() %>%
#         left_join(closures) %>%
#         filter(call_count > 1 && parameter_count > 0) %>%
#         mutate(parameter_class = classify_parameter_position(parameter_use_count, call_count)) %>%
#         group_by(function_id) %>%
#         mutate(function_class = classify_function(parameter_class)) %>%
#         ungroup() %>%
#         select(function_id, function_name, parameter_position,
#                parameter_use_count, lookup_count, metaprogram_count,
#                call_count, parameter_class, parameter_mode, function_class)
#     total_parameter_count <- nrow(classification_summary)

    irrelevant_functions <-
        unique(filter(closures, call_count == 1 | parameter_count == 0)$function_id)

    argument_use_mode_distribution <-
        analyses$`parameter-position-type` %>%
        filter(!function_id %in% irrelevant_functions) %>%
        group_by(function_id, parameter_position, argument_mode) %>%
        summarize(argument_count = sum(argument_count),
                  parameter_use_count = sum(parameter_use_count),
                  lookup_count = sum(lookup_count),
                  metaprogram_count = sum(metaprogram_count),
                  unused_count = sum(unused_count),
                  lookup_and_metaprogram_count = sum(lookup_and_metaprogram_count),
                  ## lookup_count = sum(as.logical(lookup_count)),
                  ##                  metaprogram_count = sum(as.logical(metaprogram_count)),
                  ##                  argument_mode = parameter_mode,
                  parameter_mode = compute_parameter_mode(argument_mode)) %>%
        ungroup() %>%
        left_join(closures) %>%
        filter(call_count > 1 | parameter_count > 0) %>%
        mutate(parameter_class = classify_parameter_position(parameter_use_count, call_count)) %>%
        group_by(parameter_class, argument_mode) %>%
        summarize(argument_count = sum(argument_count),
                  parameter_use_count = sum(parameter_use_count),
                  lookup_count = sum(lookup_count),
                  metaprogram_count = sum(metaprogram_count),
                  unused_count = sum(unused_count)) %>%
                  gather(argument_use_mode, argument_use_count, -argument_count,
                         -parameter_class, -argument_mode, -argument_count,
                         -parameter_use_count) %>%
                  mutate(argument_use_mode = ifelse(argument_use_mode == "lookup_count", "Lookup",
                                              ifelse(argument_use_mode == "metaprogram_count", "Metaprogram",
                                              ifelse(argument_use_mode == "unused_count", "Unused", "Lookup + Metaprogram")))) %>%
                 mutate(total_argument_count = total_argument_count) %>%
                 mutate(relative_argument_count = argument_use_count / total_argument_count)

    argument_use_mode_distribution_summary <-
        tibble(argument_use_mode = c("Lookup", "Metaprogram",
                                     "Lookup + Metaprogram", "Unused"),
               argument_count = c(sum(classification_summary$lookup_count),
                                  sum(classification_summary$metaprogram_count),
                                  sum(classification_summary$lookup_and_metaprogram_count),
                                  sum(classification_summary$unused_count))) %>%
        mutate(total_argument_count = total_argument_count) %>%
        mutate(relative_argument_count = argument_count / total_argument_count)

#        classification_summary %>%
#        select(-parameter_use_count) %>%
#        gather(parameter_use_mode, parameter_use_count, -argument_count,
#               -function_id, -function_name, -parameter_position,
#               -call_count, -parameter_class, -argument_mode) %>%
#        mutate(parameter_use_mode = ifelse(parameter_use_mode == "lookup_count", "Lookup",
#                                    ifelse(parameter_use_mode == "metaprogram_count", "Metaprogram",
#                                    ifelse(parameter_use_mode == "unused_count", "Unused", "Lookup + Metaprogram"))))
#
#     argument_use_mode_distribution_summary <-
#         argument_use_mode_distribution %>%
#         group_by(parameter_class, argument_mode, parameter_use_mode) %>%
#         summarize(parameter_use_count = sum(parameter_use_count),
#                   argument_count = sum(argument_count)) %>%
#         ungroup() %>%
#         mutate(total_argument_count = total_argument_count) %>%
#         mutate(relative_argument_count = argument_count / total_argument_count)

    function_class_summary_distribution <-
        classification_summary %>%
        group_by(function_id) %>%
        summarize(function_class = first(function_class),
                  call_count = first(call_count)) %>%
        ungroup() %>%
        group_by(function_class) %>%
        summarize(function_count = n(),
                  call_count = sum(call_count)) %>%
        ungroup() %>%
        add_row(function_class = "Zero",
                function_count = zero_argument_closure_count,
                call_count = zero_argument_closure_call_count) %>%
        add_row(function_class = "Once",
                function_count = called_once_closure_count,
                call_count = called_once_closure_call_count) %>%
        mutate(total_function_count = total_function_count,
               total_call_count = total_call_count) %>%
        mutate(relative_function_count = function_count / total_function_count,
               relative_call_count = call_count / total_call_count)

    parameter_class_summary_distribution <-
        classification_summary %>%
        group_by(parameter_class, parameter_mode) %>%
        summarize(parameter_count = n()) %>%
        mutate(total_parameter_count = total_parameter_count) %>%
        mutate(relative_parameter_count = parameter_count / total_parameter_count)

    argument_class_distribution_summary <-
        tibble(argument_class = c("Used", "Unused"),
               argument_count = c(sum(classification_summary$parameter_use_count),
                                  sum(classification_summary$call_count - classification_summary$parameter_use_count))) %>%
        mutate(total_argument_count = total_argument_count) %>%
        mutate(relative_argument_count = argument_count / total_argument_count)

    parameter_mode_distribution_summary <-
        classification_summary %>%
        group_by(parameter_mode) %>%
        summarize(parameter_count = n()) %>%
        mutate(total_parameter_count = total_parameter_count) %>%
        mutate(relative_parameter_count = parameter_count / total_parameter_count)

    argument_mode_distribution_summary <-
        analyses$`parameter-position-type` %>%
        group_by(argument_mode) %>%
        summarize(argument_count = sum(argument_count)) %>%
        mutate(total_argument_count = total_argument_count) %>%
        mutate(relative_argument_count = argument_count / total_argument_count)

    print(list(closures = closures,
               closure_call_count_summary_distribution = closure_call_count_summary_distribution,
               closure_argument_count_summary_distribution = closure_argument_count_summary_distribution,
               classification_summary = classification_summary,
               argument_use_mode_distribution = argument_use_mode_distribution,
               argument_use_mode_distribution_summary = argument_use_mode_distribution_summary,
               function_class_summary_distribution = function_class_summary_distribution,
               parameter_class_summary_distribution = parameter_class_summary_distribution,
               argument_class_distribution_summary = argument_class_distribution_summary,
               parameter_mode_distribution_summary = parameter_mode_distribution_summary,
               argument_mode_distribution_summary = argument_mode_distribution_summary,
               closure_force_order_distribution = closure_force_order_distribution,
               closure_force_order_distribution_summary = closure_force_order_distribution_summary))
}

visualize_analyses <- function(analyses) {

    total_function_count <-
        first(analyses$closure_call_count_summary_distribution$total_function_count)

    print(total_function_count)

    closure_call_distribution <-
        analyses$closure_call_count_summary_distribution %>%
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

    total_function_count <-
        first(analyses$closure_argument_count_summary_distribution$total_function_count)

    closure_parameter_count_distribution <-
        analyses$closure_argument_count_summary_distribution %>%
        ggplot(aes(parameter_count, relative_function_count)) +
        geom_col() +
        scale_x_discrete(limits = c(0:10, "> 10")) +
        scale_y_continuous(sec.axis = sec_axis(~. * total_function_count,
                                               labels = count_labels),
                           labels = relative_labels) +
        labs(x = "Parameter count",
             y = "Function count",
             title =  "Function count distribution by parameter count") +
        scale_fill_gdocs()

    total_function_count <-
        first(analyses$closure_force_order_distribution$total_function_count)

    closure_force_order_distribution <-
        analyses$closure_force_order_distribution_summary %>%
        ggplot(aes(force_order_count, relative_function_count)) +
        geom_col() +
        scale_x_discrete(limits = c(1:10, "> 10")) +
        scale_y_continuous(sec.axis = sec_axis(~ . * total_function_count,
                                               labels = count_labels),
                           labels = relative_labels) +
        labs(x = "Force order count",
             y = "Function count",
             title =  "Function count distribution by order count") +
        scale_fill_gdocs()

    total_parameter_count <-
        analyses$parameter_class_summary_distribution$total_parameter_count %>%
        first()

    parameter_mode_distribution <-
        analyses$parameter_mode_distribution_summary %>%
        ggplot(aes(x = parameter_mode,
                   y = relative_parameter_count)) +
        geom_col() +
        scale_y_continuous(sec.axis = sec_axis(~ . * total_parameter_count,
                                               labels = count_labels),
                           labels = relative_labels) +
        labs(x = "Parameter Mode",
             y = "Parameter Count (%)",
             title =  "Parameter count distribution by parameter mode") +
        scale_fill_gdocs() +
        theme(axis.text.x = element_text(angle = 60, hjust = 1))

    total_argument_count <-
        analyses$argument_mode_distribution_summary$total_argument_count %>%
        first()

    argument_mode_distribution <-
        analyses$argument_mode_distribution_summary %>%
        ggplot(aes(x = argument_mode,
                   y = relative_argument_count)) +
        geom_col() +
        scale_y_continuous(sec.axis = sec_axis(~ . * total_argument_count,
                                               labels = count_labels),
                           labels = relative_labels) +
        labs(x = "Argument Mode",
             y = "Argument Count (%)",
             title =  "Argument count distribution by argument mode") +
        scale_fill_gdocs() +
        theme(axis.text.x = element_text(angle = 60, hjust = 1))

    total_argument_count <-
        analyses$argument_use_mode_distribution_summary$total_argument_count %>%
        first()

    argument_use_mode_distribution <-
        analyses$argument_use_mode_distribution_summary %>%
        ggplot(aes(x = argument_use_mode,
                   y = relative_argument_count)) +
        geom_col() +
        scale_y_continuous(sec.axis = sec_axis(~ . * total_argument_count,
                                               labels = count_labels),
                           labels = relative_labels) +
        labs(x = "Argument Use Mode",
             y = "Argument Count (%)",
             title =  "Argument count distribution by argument use mode") +
        scale_fill_gdocs() +
        theme(axis.text.x = element_text(angle = 60, hjust = 1))

    parameter_class_distribution <-
        analyses$parameter_class_summary_distribution %>%
        ggplot(aes(x = parameter_class,
                   y = relative_parameter_count,
                   fill = parameter_mode)) +
        geom_col() +
        scale_y_continuous(sec.axis = sec_axis(~ . * total_parameter_count,
                                               labels = count_labels),
                           labels = relative_labels) +
        labs(x = "Parameter Class",
             y = "Parameter Count (%)",
             title =  "Parameter count distribution by parameter class") +
        scale_fill_gdocs() +
        theme(axis.text.x = element_text(angle = 60, hjust = 1))

    total_argument_count <-
        analyses$argument_class_distribution_summary$total_argument_count %>%
        first()

    argument_class_distribution <-
        analyses$argument_class_distribution_summary %>%
        ggplot(aes(x = argument_class,
                   y = relative_argument_count)) +
        geom_col() +
        scale_y_continuous(sec.axis = sec_axis(~ . * total_argument_count,
                                               labels = count_labels),
                           labels = relative_labels) +
        labs(x = "Argument Class",
             y = "Argument Count (%)",
             title =  "Argument count distribution by argument class") +
        scale_fill_gdocs() +
        theme(axis.text.x = element_text(angle = 60, hjust = 1))
  
      argument_count_by_mode_class_and_use <-
          analyses$argument_use_mode_distribution %>%
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
               title =  "Argument count distribution by use mode per parameter class") +
          scale_fill_gdocs() +
          theme(axis.text.x = element_text(angle = 60, hjust = 1))
  
    total_function_count <-
        analyses$function_class_summary_distribution$total_function_count %>%
        first()

    total_call_count <-
        analyses$function_class_summary_distribution$total_call_count %>%
        first()

    function_class_distribution <-
        analyses$function_class_summary_distribution %>%
        ggplot(aes(function_class, relative_function_count)) +
        geom_col() +
        scale_y_continuous(sec.axis = sec_axis(~ . * total_function_count,
                                               labels = count_labels),
                           labels = relative_labels) +
        labs(x = "Function Class",
             y = "Function Count (%)",
             title =  "Function class distribution") +
        scale_fill_gdocs() +
        theme(axis.text.x = element_text(angle = 60, hjust = 1))

    function_class_call_distribution <-
        analyses$function_class_summary_distribution %>%
        ggplot(aes(function_class, relative_call_count)) +
        geom_col() +
        scale_y_continuous(sec.axis = sec_axis(~ . * total_call_count,
                                               labels = count_labels),
                           labels = relative_labels) +
        labs(x = "Function class",
             y = "Call Count (%)",
             title =  "Call count distribution by function class") +
        scale_fill_gdocs() +
        theme(axis.text.x = element_text(angle = 60, hjust = 1))

    list(parameter_class_distribution = parameter_class_distribution,
         argument_class_distribution = argument_class_distribution,
         parameter_mode_distribution = parameter_mode_distribution,
         function_class_distribution = function_class_distribution,
         function_class_call_distribution = function_class_call_distribution,
         argument_class_distribution = argument_class_distribution,
         argument_mode_distribution = argument_mode_distribution,
         argument_use_mode_distribution = argument_use_mode_distribution,
         argument_count_by_mode_class_and_use = argument_count_by_mode_class_and_use,
         closure_call_distribution = closure_call_distribution,
         closure_parameter_count_distribution = closure_parameter_count_distribution,
         closure_force_order_distribution = closure_force_order_distribution)
 ## closure_argument_count_distribution = closure_argument_count_distribution)

}


latex_analyses <- function(analyses) {
    list()
}

main <- function() {
    analyzer <-
        create_analyzer("Parameter Usage Analysis",
                        c("parameter-force-order", "parameter-usage-count", "functions"),
                        reduce_analysis,
                        combine_analyses,
                        summarize_analyses,
                        visualize_analyses,
                        latex_analyses)
    drive_analysis(analyzer)
}

main()
summary(warnings())
