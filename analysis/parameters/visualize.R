#!/usr/bin/env Rscript

## WARN - no good way in R to find path of current script
##        perhaps the only good solution is to create a
##        package for the analysis ?

##force_order_count should be changed to fore_order_call_count
##also add force_order_count field to the main function table.

options(error = quote({dump.frames(to.file=FALSE); q();}))
options( warn = 2)
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


object_type <- function(analyses) {

    total_object_count <-
        analyses$object_count_by_type %>%
        pull(count) %>%
        sum()

    object_count_by_type <-
        analyses$object_count_by_type %>%
        ggplot(aes(x = type, y = relative_count)) +
        geom_col() +
        scale_y_continuous(sec.axis = sec_axis(~ . * total_object_count,
                                               labels = count_labels),
                           labels = relative_labels) +
        labs(x = "Object Type",
             y = "Object Count",
             title =  "Object count by Object type") +
        scale_fill_gdocs() +
        theme(axis.text.x = element_text(angle = 60, hjust = 1))

    list(object_count_by_type = object_count_by_type)
}


promise_use_and_action <- function(analyses) {

    total_promise_count <-
        analyses$promise_use_distribution_by_category %>%
        pull(promise_count) %>%
        sum()

    promise_use_distribution_by_category <-
        analyses$promise_use_distribution_by_category %>%
        ggplot(aes(x = use,
                   y = relative_promise_count,
                   fill = promise_category)) +
        geom_col(position = "dodge") +
        scale_y_continuous(sec.axis = sec_axis(~ . * total_promise_count,
                                               labels = count_labels),
                           labels = relative_labels) +
        labs(x = "Promise Use",
             y = "Promise Count",
             title =  "Promise use distribution by category") +
        scale_fill_gdocs() +
        theme(legend.position = "bottom")

    total_promise_count <-
        analyses$promise_action_distribution_by_category %>%
        pull(promise_count) %>%
        sum()

    promise_action_distribution_by_category <-
        analyses$promise_action_distribution_by_category %>%
        ggplot(aes(x = action,
                   y = relative_promise_count,
                   fill = promise_category)) +
        geom_col(position = "dodge") +
        scale_y_continuous(sec.axis = sec_axis(~ . * total_promise_count,
                                               labels = count_labels),
                           labels = relative_labels) +
        labs(x = "Promise Action",
             y = "Promise Count",
             title =  "Promise action distribution by category") +
        scale_fill_gdocs() +
        theme(legend.position = "bottom")


###promise_action_distribution_by_category = promise_action_distribution_by_category

    list(promise_use_distribution_by_category = promise_use_distribution_by_category,
         promise_action_distribution_by_category = promise_action_distribution_by_category)
}


parameters <- function(analyses) {
    ## formal_parameter_count_by_usage_class = formal_parameter_count_by_usage_class,
    ## function_count_distribution_by_usage_class = function_count_distribution_by_usage_class

    total_argument_count <-
        analyses$argument_count_by_usage %>%
        pull(argument_count) %>%
        sum()

    argument_count_by_usage <-
        analyses$argument_count_by_usage %>%
        ggplot(aes(x = argument_use,
                   y = relative_argument_count)) +
        geom_col() +
        scale_y_continuous(sec.axis = sec_axis(~ . * total_argument_count,
                                               labels = count_labels),
                           labels = relative_labels) +
        labs(x = "Argument Use",
             y = "Argument Count",
             title =  "Argument Count by Usage") +
        scale_fill_gdocs()

    total_parameter_count <-
        analyses$formal_parameter_count_by_usage %>%
        pull(parameter_count) %>%
        sum()

    formal_parameter_count_by_usage <-
        analyses$formal_parameter_count_by_usage %>%
        ggplot(aes(x = parameter_use,
                   y = relative_parameter_count)) +
        geom_col() +
        scale_y_continuous(sec.axis = sec_axis(~ . * total_parameter_count,
                                               labels = count_labels),
                           labels = relative_labels) +
        labs(x = "Parameter Use",
             y = "Parameter Count",
             title =  "Parameter Count by Usage") +
        scale_fill_gdocs()

    formal_parameter_count_by_usage_class <-
        analyses$formal_parameter_count_by_usage_class %>%
        ggplot(aes(x = parameter_class,
                   y = relative_parameter_count)) +
        geom_col() +
        facet_wrap(~ parameter_use) +
        scale_y_continuous(sec.axis = sec_axis(~ . * total_parameter_count,
                                               labels = count_labels),
                           labels = relative_labels) +
        labs(x = "Parameter Class",
             y = "Parameter Count",
             title =  "Parameter count by class") +
        scale_fill_gdocs()

    closure_count_distribution_by_usage_class <-
        analyses$closure_count_distribution_by_usage_class %>%
        ggplot(aes(x = closure_class,
                   y = closure_count)) +
        geom_col() +
        facet_wrap(~ closure_use) +
        labs(x = "Function Class",
             y = "Function Count",
             title =  "Parameter count by class") +
        scale_fill_gdocs() +
        theme(axis.text.x = element_text(angle = 60, hjust = 1),
              legend.position = "bottom")

    list(argument_count_by_usage = argument_count_by_usage,
         formal_parameter_count_by_usage = formal_parameter_count_by_usage,
         formal_parameter_count_by_usage_class = formal_parameter_count_by_usage_class,
         closure_count_distribution_by_usage_class = closure_count_distribution_by_usage_class)
}


functions <- function(analyses) {

    total_function_count <-
        analyses$function_count_by_type %>%
        pull(function_count) %>%
        sum()

    total_call_count <-
        analyses$function_call_count_by_type %>%
        pull(call_count) %>%
        sum()

    total_closure_count <-
        analyses$function_count_by_type %>%
        filter(function_type == "Closure") %>%
        pull(function_count) %>%
        sum()

    total_special_count <-
        analyses$function_count_by_type %>%
        filter(function_type == "Special") %>%
        pull(function_count) %>%
        sum()

    total_builtin_count <-
        analyses$function_count_by_type %>%
        filter(function_type == "Builtin") %>%
        pull(function_count) %>%
        sum()

    closure_count_by_call_count <-
        analyses$closure_count_by_call_count %>%
        filter(call_count %in%  c(1:10, "> 10")) %>%
        ggplot(aes(x = call_count,
                   y = relative_closure_count)) +
        geom_col() +
        scale_x_discrete(limits = c(1:10, "> 10")) +
        scale_y_continuous(sec.axis = sec_axis(~ . * total_closure_count,
                                               labels = count_labels),
                           labels = relative_labels) +
        labs(x = "Call Count",
             y = "Closure Count",
             title =  "Closure count by Call count") +
        scale_fill_gdocs()


    function_count_by_type <-
        analyses$function_count_by_type %>%
        ggplot(aes(x = function_type,
                   y = relative_function_count)) +
        geom_col() +
        scale_y_continuous(sec.axis = sec_axis(~ . * total_function_count,
                                               labels = count_labels),
                           labels = relative_labels) +
        labs(x = "Function Type",
             y = "Function Count",
             title =  "Function Count by Function Type") +
        scale_fill_gdocs()

    function_call_count_by_type <-
        analyses$function_call_count_by_type %>%
        ggplot(aes(x = function_type,
                   y = relative_call_count)) +
        geom_col() +
        scale_y_continuous(sec.axis = sec_axis(~ . * total_call_count,
                                               labels = count_labels),
                           labels = relative_labels) +
        labs(x = "Function Type",
             y = "Call Count",
             title =  "Call Count by Function Type") +
        scale_fill_gdocs()

    function_call_count_by_return_value_type <-
        analyses$function_call_count_by_return_value_type %>%
        ggplot(aes(x = return_value_type,
                   y = relative_call_count,
                   fill = function_type)) +
        geom_col(position = "dodge") +
        scale_y_continuous(sec.axis = sec_axis(~ . * total_call_count,
                                               labels = count_labels),
                           labels = relative_labels) +
        labs(x = "Return Value Type",
             y = "Call Count",
             title =  "Function Call Count by Return Value Type") +
        scale_fill_gdocs() +
        theme(axis.text.x = element_text(angle = 60, hjust = 1),
              legend.position = "bottom")

    builtin_count_by_return_value_type <-
        analyses$function_count_by_return_value_type %>%
        filter(function_type == "Builtin") %>%
        ggplot(aes(x = return_value_type,
                   y = relative_function_count)) +
        geom_col(position = "dodge") +
        scale_y_continuous(sec.axis = sec_axis(~ . * total_builtin_count,
                                               labels = count_labels),
                           labels = relative_labels) +
        labs(x = "Return Value Type",
             y = "Function Count",
             title =  "Builtin function Count by Return Value Type") +
        scale_fill_gdocs() +
        theme(axis.text.x = element_text(angle = 60, hjust = 1),
              legend.position = "bottom")

    special_count_by_return_value_type <-
        analyses$function_count_by_return_value_type %>%
        filter(function_type == "Special") %>%
        ggplot(aes(x = return_value_type,
                   y = relative_function_count)) +
        geom_col(position = "dodge") +
        scale_y_continuous(sec.axis = sec_axis(~ . * total_special_count,
                                               labels = count_labels),
                           labels = relative_labels) +
        labs(x = "Return Value Type",
             y = "Function Count",
             title =  "Special function Count by Return Value Type") +
        scale_fill_gdocs() +
        theme(axis.text.x = element_text(angle = 60, hjust = 1),
              legend.position = "bottom")

    closure_count_by_return_value_type <-
        analyses$function_count_by_return_value_type %>%
        filter(function_type == "Closure") %>%
        ggplot(aes(x = return_value_type,
                   y = relative_function_count)) +
        geom_col(position = "dodge") +
        scale_y_continuous(sec.axis = sec_axis(~ . * total_closure_count,
                                               labels = count_labels),
                           labels = relative_labels) +
        labs(x = "Return Value Type",
             y = "Function Count",
             title =  "Closure function Count by Return Value Type") +
        scale_fill_gdocs() +
        theme(axis.text.x = element_text(angle = 60, hjust = 1),
              legend.position = "bottom")


    closure_count_by_formal_parameter_count <-
        analyses$closure_count_by_formal_parameter_count %>%
        filter(formal_parameter_count %in% c(0:5, "> 5")) %>%
        ggplot(aes(x = formal_parameter_count,
                   y = relative_closure_count)) +
        geom_col() +
        scale_x_discrete(limits = c(0:5, "> 5")) +
        scale_y_continuous(sec.axis = sec_axis(~ . * total_closure_count,
                                               labels = count_labels),
                           labels = relative_labels) +
        labs(x = "Parameter Count",
             y = "Closure Count",
             title =  "Closure Count by Parameter Count") +
        scale_fill_gdocs()

    function_call_count_by_type_and_jump <-
        analyses$function_call_count_by_type_and_jump %>%
        ggplot(aes(x = jumped,
                   y = relative_call_count,
                   fill = function_type)) +
        geom_col(position = "dodge") +
        scale_y_continuous(sec.axis = sec_axis(~ . * total_call_count,
                                               labels = count_labels),
                           labels = relative_labels) +
        labs(x = "Jumped",
             y = "Call Count",
             title =  "Call count by type and jump") +
        scale_fill_gdocs()

    function_call_count_by_method_type <-
        analyses$function_call_count_by_method_type %>%
        ggplot(aes(x = method_type,
                   y = relative_call_count)) +
        geom_col(position = "dodge") +
        facet_wrap(~ function_type) +
        scale_y_continuous(sec.axis = sec_axis(~ . * total_call_count,
                                               labels = count_labels),
                           labels = relative_labels) +
        labs(x = "Method Type",
             y = "Call Count",
             title =  "Function Call Count by Method Type") +
        scale_fill_gdocs() +
        theme(axis.text.x = element_text(angle = 60, hjust = 1),
              legend.position = "bottom")

    function_count_by_method_type <-
        analyses$function_count_by_method_type %>%
        ggplot(aes(x = method_type,
                   y = relative_function_count)) +
        geom_col(position = "dodge") +
        facet_wrap(~ function_type) +
        scale_y_continuous(sec.axis = sec_axis(~ . * total_function_count,
                                               labels = count_labels),
                           labels = relative_labels) +
        labs(x = "Method Type",
             y = "Function Count",
             title =  "Function Count by Method Type") +
        scale_fill_gdocs() +
        theme(axis.text.x = element_text(angle = 60, hjust = 1),
              legend.position = "bottom")

    closure_count_by_wrapper_and_force_order <-
        analyses$closure_count_by_wrapper_and_force_order %>%
        filter(force_order_count %in% c(0:5, "> 5")) %>%
        ggplot(aes(x = force_order_count,
                   y = relative_closure_count,
                   fill = force_order_type)) +
        geom_col(position = "dodge") +
        facet_wrap(~ wrapper) +
        scale_x_discrete(limits = c(0:5, "> 5")) +
        scale_y_continuous(sec.axis = sec_axis(~ . * total_closure_count,
                                               labels = count_labels),
                           labels = relative_labels) +
        labs(x = "Force Orders",
             y = "Closure Count",
             title =  "Closure Count by Force Order Count") +
        scale_fill_gdocs() +
        theme(legend.position = "bottom")

    list(closure_count_by_call_count = closure_count_by_call_count,
         function_count_by_type = function_count_by_type,
         function_call_count_by_type = function_call_count_by_type,
         function_call_count_by_return_value_type = function_call_count_by_return_value_type,
         builtin_count_by_return_value_type = builtin_count_by_return_value_type,
         special_count_by_return_value_type = special_count_by_return_value_type,
         closure_count_by_return_value_type = closure_count_by_return_value_type,
         closure_count_by_formal_parameter_count = closure_count_by_formal_parameter_count,
         function_call_count_by_type_and_jump = function_call_count_by_type_and_jump,
         function_call_count_by_method_type = function_call_count_by_method_type,
         function_count_by_method_type = function_count_by_method_type,
         closure_count_by_wrapper_and_force_order = closure_count_by_wrapper_and_force_order)
}


escaped_arguments <- function(analyses) {

    total_call_count <-
        analyses$escaped_argument_function_call_count_by_return_value_type %>%
        pull(call_count) %>%
        sum()

    total_function_count <-
        analyses$escaped_argument_function_count_by_return_value_type %>%
        pull(function_count) %>%
        sum()

    escaped_argument_function_call_count_by_return_value_type <-
        analyses$escaped_argument_function_call_count_by_return_value_type %>%
        ggplot(aes(x = return_value_type,
                   y = relative_call_count)) +
        geom_col() +
        scale_y_continuous(sec.axis = sec_axis(~ . * total_call_count,
                                               labels = count_labels),
                           labels = relative_labels) +
        labs(x = "Return Value Type",
             y = "Call Count",
             title =  "Escaped Argument Function Call Count by Return Value Type") +
        scale_fill_gdocs() +
        theme(axis.text.x = element_text(angle = 60, hjust = 1),
              legend.position = "bottom")


    escaped_argument_function_count_by_return_value_type <-
        analyses$escaped_argument_function_count_by_return_value_type %>%
        ggplot(aes(x = return_value_type,
                   y = relative_function_count)) +
        geom_col() +
        scale_y_continuous(sec.axis = sec_axis(~ . * total_function_count,
                                               labels = count_labels),
                           labels = relative_labels) +
        labs(x = "Return Value Type",
             y = "Function Count",
             title =  "Escaped Argument Function Count by Return Value Type") +
        scale_fill_gdocs() +
        theme(axis.text.x = element_text(angle = 60, hjust = 1),
              legend.position = "bottom")

    escaped_argument_function_count_by_category <-
        analyses$escaped_argument_function_count_by_category %>%
        ggplot(aes(x = category,
                   y = relative_function_count)) +
        geom_col() +
        scale_y_continuous(sec.axis = sec_axis(~ . * total_function_count,
                                               labels = count_labels),
                           labels = relative_labels) +
        labs(x = "Function Category",
             y = "Function Count",
             title =  "Escaped Argument Function Category by Return Value Type") +
        scale_fill_gdocs() +
        theme(axis.text.x = element_text(angle = 60, hjust = 1),
              legend.position = "bottom")

    total_argument_count <-
        analyses$escaped_argument_count_by_nature %>%
        pull(argument_count) %>%
        sum()

    escaped_argument_count_by_nature <-
        analyses$escaped_argument_count_by_nature %>%
        ggplot(aes(x = argument_nature,
                   y = relative_argument_count)) +
        geom_col() +
        scale_y_continuous(sec.axis = sec_axis(~ . * total_argument_count,
                                               labels = count_labels),
                           labels = relative_labels) +
        labs(x = "Argument Nature",
             y = "Argument Count",
             title =  "Escaped Argument count by nature") +
        scale_fill_gdocs() +
        theme(axis.text.x = element_text(angle = 60, hjust = 1),
              legend.position = "bottom")

    escaped_argument_count_by_dispatch_type <-
        analyses$escaped_argument_count_by_dispatch_type %>%
        ggplot(aes(x = dispatch_type,
                   y = relative_argument_count)) +
        geom_col() +
        scale_y_continuous(sec.axis = sec_axis(~ . * total_argument_count,
                                               labels = count_labels),
                           labels = relative_labels) +
        labs(x = "Dispatch Type",
             y = "Argument Count",
             title =  "Escaped Argument count by dispatch type") +
        scale_fill_gdocs() +
        theme(axis.text.x = element_text(angle = 60, hjust = 1),
              legend.position = "bottom")


    escaped_argument_count_by_expression_type <-
        analyses$escaped_argument_count_by_expression_type %>%
        ggplot(aes(x = expression_type,
                   y = relative_argument_count)) +
        geom_col() +
        scale_y_continuous(sec.axis = sec_axis(~ . * total_argument_count,
                                               labels = count_labels),
                           labels = relative_labels) +
        labs(x = "Expression Type",
             y = "Escaped Argument Count",
             title =  "Escaped Argument count by expression type") +
        scale_fill_gdocs() +
        theme(axis.text.x = element_text(angle = 60, hjust = 1),
              legend.position = "bottom")

    escaped_argument_count_by_value_type <-
        analyses$escaped_argument_count_by_value_type %>%
        ggplot(aes(x = value_type,
                   y = relative_argument_count)) +
        geom_col() +
        scale_y_continuous(sec.axis = sec_axis(~ . * total_argument_count,
                                               labels = count_labels),
                           labels = relative_labels) +
        labs(x = "Value Type",
             y = "Escaped Argument Count",
             title =  "Escaped Argument count by value type") +
        scale_fill_gdocs() +
        theme(axis.text.x = element_text(angle = 60, hjust = 1),
              legend.position = "bottom")

    escaped_argument_count_by_force_type <-
        analyses$escaped_argument_count_by_force_type %>%
        ggplot(aes(x = force_type,
                   y = relative_argument_count)) +
        geom_col() +
        scale_y_continuous(sec.axis = sec_axis(~ . * total_argument_count,
                                               labels = count_labels),
                           labels = relative_labels) +
        labs(x = "Force Type",
             y = "Escaped Argument Count",
             title =  "Escaped Argument count by force type") +
        scale_fill_gdocs() +
        theme(axis.text.x = element_text(angle = 60, hjust = 1),
              legend.position = "bottom")


    escaped_argument_count_by_metaprogram_type <-
        analyses$escaped_argument_count_by_metaprogram_type %>%
        ggplot(aes(x = metaprogram_type,
                   y = relative_argument_count)) +
        geom_col() +
        scale_y_continuous(sec.axis = sec_axis(~ . * total_argument_count,
                                               labels = count_labels),
                           labels = relative_labels) +
        labs(x = "Metaprogram Type",
             y = "Escaped Argument Count",
             title =  "Escaped Argument count by metaprogram type") +
        scale_fill_gdocs() +
        theme(axis.text.x = element_text(angle = 60, hjust = 1),
              legend.position = "bottom")


    escaped_argument_count_by_lookup_type <-
        analyses$escaped_argument_count_by_lookup_type %>%
        ggplot(aes(x = lookup_type,
                   y = relative_argument_count)) +
        geom_col() +
        scale_y_continuous(sec.axis = sec_axis(~ . * total_argument_count,
                                               labels = count_labels),
                           labels = relative_labels) +
        labs(x = "Lookup Type",
             y = "Escaped Argument Count",
             title =  "Escaped Argument count by lookup type") +
        scale_fill_gdocs() +
        theme(axis.text.x = element_text(angle = 60, hjust = 1),
              legend.position = "bottom")


    visualize_before_after <- function(df, x, y, total_argument_count,
                                       x_label, y_label, title) {
        x = enquo(x)
        y = enquo(y)

        df %>%
            ggplot(aes(x = !!x, y = !!y)) +
            geom_col() +
            scale_y_continuous(sec.axis = sec_axis(~ . * total_argument_count,
                                                   labels = count_labels),
                               labels = relative_labels) +
            labs(x = x_label, y = y_label, title =  title) +
            scale_fill_gdocs() +
            theme(axis.text.x = element_text(angle = 60, hjust = 1), legend.position = "bottom")
    }

    escaped_argument_count_by_direct_self_scope_mutation_type <-
        analyses$escaped_argument_count_by_direct_self_scope_mutation_type %>%
        visualize_before_after(direct_self_scope_mutation_type,
                               relative_argument_count,
                               total_argument_count,
                               "Direct self scope mutation type",
                               "Escaped argument count",
                               "Escaped argument count by direct self scope mutation type")


    escaped_argument_count_by_indirect_self_scope_mutation_type <-
        analyses$escaped_argument_count_by_indirect_self_scope_mutation_type %>%
        visualize_before_after(indirect_self_scope_mutation_type,
                               relative_argument_count,
                               total_argument_count,
                               "Indirect self scope mutation type",
                               "Escaped argument count",
                               "Escaped argument count by indirect self scope mutation type")

    escaped_argument_count_by_direct_lexical_scope_mutation_type <-
        analyses$escaped_argument_count_by_direct_lexical_scope_mutation_type %>%
        visualize_before_after(direct_lexical_scope_mutation_type,
                               relative_argument_count,
                               total_argument_count,
                               "Direct lexical scope mutation type",
                               "Escaped argument count",
                               "Escaped argument count by direct lexical scope mutation type")

    escaped_argument_count_by_indirect_lexical_scope_mutation_type <-
        analyses$escaped_argument_count_by_indirect_lexical_scope_mutation_type %>%
        visualize_before_after(indirect_lexical_scope_mutation_type,
                               relative_argument_count,
                               total_argument_count,
                               "Indirect lexical scope mutation type",
                               "Escaped argument count",
                               "Escaped argument count by indirect lexical scope mutation type")

    escaped_argument_count_by_direct_non_lexical_scope_mutation_type <-
        analyses$escaped_argument_count_by_direct_non_lexical_scope_mutation_type %>%
        visualize_before_after(direct_non_lexical_scope_mutation_type,
                               relative_argument_count,
                               total_argument_count,
                               "Direct non lexical scope mutation type",
                               "Escaped argument count",
                               "Escaped argument count by direct non lexical scope mutation type")

    escaped_argument_count_by_indirect_non_lexical_scope_mutation_type <-
        analyses$escaped_argument_count_by_indirect_non_lexical_scope_mutation_type %>%
        visualize_before_after(indirect_non_lexical_scope_mutation_type,
                               relative_argument_count,
                               total_argument_count,
                               "Indirect non lexical scope mutation type",
                               "Escaped argument count",
                               "Escaped argument count by indirect non lexical scope mutation type")

    escaped_argument_count_by_direct_self_scope_observation_type <-
        analyses$escaped_argument_count_by_direct_self_scope_observation_type %>%
        visualize_before_after(direct_self_scope_observation_type,
                               relative_argument_count,
                               total_argument_count,
                               "Direct self scope observation type",
                               "Escaped argument count",
                               "Escaped argument count by direct self scope observation type")

    escaped_argument_count_by_indirect_self_scope_observation_type <-
        analyses$escaped_argument_count_by_indirect_self_scope_observation_type %>%
        visualize_before_after(indirect_self_scope_observation_type,
                               relative_argument_count,
                               total_argument_count,
                               "Indirect self scope observation type",
                               "Escaped argument count",
                               "Escaped argument count by indirect self scope observation type")

    escaped_argument_count_by_direct_lexical_scope_observation_type <-
        analyses$escaped_argument_count_by_direct_lexical_scope_observation_type %>%
        visualize_before_after(direct_lexical_scope_observation_type,
                               relative_argument_count,
                               total_argument_count,
                               "Direct lexical scope observation type",
                               "Escaped argument count",
                               "Escaped argument count by direct lexical scope observation type")

    escaped_argument_count_by_indirect_lexical_scope_observation_type <-
        analyses$escaped_argument_count_by_indirect_lexical_scope_observation_type %>%
        visualize_before_after(indirect_lexical_scope_observation_type,
                               relative_argument_count,
                               total_argument_count,
                               "Indirect lexical scope observation type",
                               "Escaped argument count",
                               "Escaped argument count by indirect lexical scope observation type")

    escaped_argument_count_by_direct_non_lexical_scope_observation_type <-
        analyses$escaped_argument_count_by_direct_non_lexical_scope_observation_type %>%
        visualize_before_after(direct_non_lexical_scope_observation_type,
                               relative_argument_count,
                               total_argument_count,
                               "Direct non lexical scope observation type",
                               "Escaped argument count",
                               "Escaped argument count by direct non lexical scope mutation type")

    escaped_argument_count_by_indirect_non_lexical_scope_observation_type <-
        analyses$escaped_argument_count_by_indirect_non_lexical_scope_observation_type %>%
        visualize_before_after(indirect_non_lexical_scope_observation_type,
                               relative_argument_count,
                               total_argument_count,
                               "Indirect non lexical scope observation type",
                               "Escaped argument count",
                               "Escaped argument count by indirect non lexical scope observation type")

    list(escaped_argument_function_call_count_by_return_value_type = escaped_argument_function_call_count_by_return_value_type,
         escaped_argument_function_count_by_return_value_type = escaped_argument_function_count_by_return_value_type,
         escaped_argument_function_count_by_category = escaped_argument_function_count_by_category,
         escaped_argument_count_by_nature = escaped_argument_count_by_nature,
         escaped_argument_count_by_dispatch_type = escaped_argument_count_by_dispatch_type,
         escaped_argument_count_by_expression_type = escaped_argument_count_by_expression_type,
         escaped_argument_count_by_value_type = escaped_argument_count_by_value_type,
         escaped_argument_count_by_force_type = escaped_argument_count_by_force_type,
         escaped_argument_count_by_metaprogram_type = escaped_argument_count_by_metaprogram_type,
         escaped_argument_count_by_lookup_type = escaped_argument_count_by_lookup_type,
         escaped_argument_count_by_direct_self_scope_mutation_type = escaped_argument_count_by_direct_self_scope_mutation_type,
         escaped_argument_count_by_indirect_self_scope_mutation_type = escaped_argument_count_by_indirect_self_scope_mutation_type,
         escaped_argument_count_by_direct_lexical_scope_mutation_type = escaped_argument_count_by_direct_lexical_scope_mutation_type,
         escaped_argument_count_by_indirect_lexical_scope_mutation_type = escaped_argument_count_by_indirect_lexical_scope_mutation_type,
         escaped_argument_count_by_direct_non_lexical_scope_mutation_type = escaped_argument_count_by_direct_non_lexical_scope_mutation_type,
         escaped_argument_count_by_indirect_non_lexical_scope_mutation_type = escaped_argument_count_by_indirect_non_lexical_scope_mutation_type,
         escaped_argument_count_by_direct_self_scope_observation_type = escaped_argument_count_by_direct_self_scope_observation_type,
         escaped_argument_count_by_indirect_self_scope_observation_type = escaped_argument_count_by_indirect_self_scope_observation_type,
         escaped_argument_count_by_direct_lexical_scope_observation_type = escaped_argument_count_by_direct_lexical_scope_observation_type,
         escaped_argument_count_by_indirect_lexical_scope_observation_type = escaped_argument_count_by_indirect_lexical_scope_observation_type,
         escaped_argument_count_by_direct_non_lexical_scope_observation_type = escaped_argument_count_by_direct_non_lexical_scope_observation_type,
         escaped_argument_count_by_indirect_non_lexical_scope_observation_type = escaped_argument_count_by_indirect_non_lexical_scope_observation_type)
}


argument_promise_relation <- function(analyses) {

    total_promise_count <-
        analyses$promise_distribution_by_category %>%
        pull(promise_count) %>%
        sum()

    promise_distribution_by_category <-
        analyses$promise_distribution_by_category %>%
        ggplot(aes(x = promise_category,
                   y = relative_promise_count)) +
        geom_col() +
        scale_y_continuous(sec.axis = sec_axis(~ . * total_promise_count,
                                               labels = count_labels),
                           labels = relative_labels) +
        labs(x = "Promise Category",
             y = "Promise Count",
             title =  "Promise distribution by category") +
        scale_fill_gdocs()

    total_argument_count <-
        analyses$argument_distribution_by_type %>%
        pull(argument_count) %>%
        sum()

    argument_distribution_by_type <-
        analyses$argument_distribution_by_type %>%
        ggplot(aes(x = reorder(argument_type, -relative_argument_count, identity),
                   y = argument_count)) +
        geom_col() +
        scale_y_log10(sec.axis = sec_axis(~ . / total_argument_count,
                                          labels = relative_labels),
                      labels = count_labels) +
        labs(x = "Argument Type",
             y = "Argument Count",
             title =  "Argument distribution by type") +
        scale_fill_gdocs()


    total_argument_count <-
        analyses$argument_distribution_by_dot_dot_dot %>%
        pull(argument_count) %>%
        sum()

    argument_distribution_by_dot_dot_dot <-
        analyses$argument_distribution_by_dot_dot_dot %>%
        ggplot(aes(x = reorder(argument_category, -relative_argument_count, identity),
                   y = relative_argument_count)) +
        geom_col() +
        scale_y_continuous(sec.axis = sec_axis(~ . * total_argument_count,
                                               labels = count_labels),
                           labels = relative_labels) +
        labs(x = "Argument Category",
             y = "Argument Count",
             title =  "Argument distribution by type") +
        scale_fill_gdocs()


    total_argument_promise_count <-
        analyses$argument_promise_distribution_by_nature %>%
        pull(argument_count) %>%
        sum()

    argument_promise_distribution_by_nature <-
        analyses$argument_promise_distribution_by_nature %>%
        ggplot(aes(x = argument_nature,
                   y = relative_argument_count)) +
        geom_col() +
        scale_y_continuous(sec.axis = sec_axis(~ . * total_argument_promise_count,
                                               labels = count_labels),
                           labels = relative_labels) +
        labs(x = "Argument Nature",
             y = "Argument Count",
             title =  "Argument promise distribution by nature") +
        scale_fill_gdocs()

    argument_promise_distribution_by_sharing <-
        analyses$argument_promise_distribution_by_sharing %>%
        filter(sharing_count %in% c(1:3, "> 3")) %>%
        ggplot(aes(x = sharing_count,
                   y = relative_promise_count)) +
        geom_col() +
        scale_x_discrete(limits = c(1:3, "> 3")) +
        scale_y_continuous(sec.axis = sec_axis(~ . * total_argument_promise_count,
                                               labels = count_labels),
                           labels = relative_labels) +
        labs(x = "Sharing Count",
             y = "Promise Count",
             title =  "Promise distribution by argument category and sharing") +
        scale_fill_gdocs() +
        theme(axis.text.x = element_text(angle = 60, hjust = 1))


    list(promise_distribution_by_category = promise_distribution_by_category,
         argument_distribution_by_type = argument_distribution_by_type,
         argument_distribution_by_dot_dot_dot = argument_distribution_by_dot_dot_dot,
         argument_promise_distribution_by_nature = argument_promise_distribution_by_nature,
         argument_promise_distribution_by_sharing = argument_promise_distribution_by_sharing)
}

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

    total_argument_count <-
        analyses$argument_count_by_call_depth %>%
        pull(argument_count) %>%
        sum()

    argument_count_by_call_depth_and_expression_type <-
        analyses$argument_count_by_call_depth_and_expression_type %>%
        ggplot(aes(x = call_depth,
                   y = relative_argument_count,
                   fill = expression_type)) +
        geom_col() +
        scale_x_discrete(limits = c(-2:10, "> 10")) +
        scale_y_continuous(sec.axis = sec_axis(~ . * total_argument_count,
                                               labels = count_labels),
                           labels = relative_labels) +
        labs(x = "Call depth",
             y = "Argument count",
             title =  "Argument Count by Call Depth") +
        ##scale_fill_gdocs() +
        theme(axis.text.x = element_text(angle = 60, hjust = 1),
              legend.position = "bottom")


    argument_count_by_promise_depth_and_expression_type <-
        analyses$argument_count_by_promise_depth_and_expression_type %>%
        ggplot(aes(x = promise_depth,
                   y = relative_argument_count,
                   fill = expression_type)) +
        geom_col() +
        scale_x_discrete(limits = c(-2:10, "> 10")) +
        scale_y_continuous(sec.axis = sec_axis(~ . * total_argument_count,
                                               labels = count_labels),
                           labels = relative_labels) +
        labs(x = "Promise depth",
             y = "Argument count",
             title =  "Argument Count by Promise Depth") +
        ##scale_fill_gdocs() +
        theme(axis.text.x = element_text(angle = 60, hjust = 1),
              legend.position = "bottom")


    argument_count_by_nested_promise_depth_and_expression_type <-
        analyses$argument_count_by_nested_promise_depth_and_expression_type %>%
        ggplot(aes(x = nested_promise_depth,
                   y = relative_argument_count,
                   fill = expression_type)) +
        geom_col() +
        ##scale_x_discrete(limits = c(-2:10, "> 10")) +
        scale_y_continuous(sec.axis = sec_axis(~ . * total_argument_count,
                                               labels = count_labels),
                           labels = relative_labels) +
        labs(x = "Nested Promise depth",
             y = "Argument count",
             title =  "Argument Count by Nested Promise Depth") +
        ##scale_fill_gdocs() +
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
         nonwrapper_function_count_by_function_class_force_order_type_and_force_order_count = nonwrapper_function_count_by_function_class_force_order_type_and_force_order_count,
         argument_count_by_call_depth_and_expression_type = argument_count_by_call_depth_and_expression_type,
         argument_count_by_promise_depth_and_expression_type = argument_count_by_promise_depth_and_expression_type,
         argument_count_by_nested_promise_depth_and_expression_type = argument_count_by_nested_promise_depth_and_expression_type)

}

visualize_summarized_data <- function(settings, summarized_data_table) {

    info("=> Starting visualization\n")

    visualizer <- eval(as.symbol(settings$analysis))

    summarized_data_filepaths <- summarized_data_table$filepath

    visualized_data_filepaths <-
        summarized_data_filepaths %>%
        map(promisedyntracer::read_data_table,
            binary = settings$binary,
            compression_level = settings$compression_level) %>%
        set_names(path_ext_remove(path_file(summarized_data_filepaths))) %>%
        visualizer() %>%
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

    ext <- data_table_extension(settings$binary, settings$compression_level)

    summarized_data_table <-
        settings$input_dirpath %>%
        dir_ls(type = "file", ext = ext) %>%
        {tibble(filepath = path_ext_remove(path_ext_remove(.)))}

    info("=> Found ", nrow(summarized_data_table), " data files\n")

    summarized_data_table
}


parse_program_arguments <- function() {

    usage <- "%prog summarized-output-dirpath visualized-output-dirpath"
    description <- paste(
        "summarized-output-dirpath  directory containing summarized data files",
        "visualized-output-dirpath  directory to which visualizations will be exported",
        "analysis                   name of analysis to visualize",
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
         binary = arguments$options$binary,
         compression_level = arguments$options$compression_level)
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


## ```{r echo = FALSE}
## show_table("formal_parameter_count_by_usage_class")
## ```

## ```{r echo = FALSE, out.width="1000px"}
## show_graph("formal_parameter_count_by_usage_class")
## ```

## ```{r echo = FALSE}
## show_table("function_count_distribution_by_usage_class")
## ```

## ```{r echo = FALSE, out.width="1000px"}
## show_graph("function_count_distribution_by_usage_class")
## ```
