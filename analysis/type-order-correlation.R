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

    closures <-
        analyses$calls %>%
        filter(function_type == "Closure")

    orders <-
        analyses$arguments %>%
        group_by(call_id) %>%
        summarize(argument_mode_order = str_c(argument_mode,
                                              collapse = " | "),
                  expression_type_order = str_c(expression_type,
                                                collapse = " | "),
                  value_type_order = str_c(value_type,
                                           collapse = " | "),
                  lookup_count_order = str_c(as.logical(lookup_count),
                                             collapse = " | "),
                  metaprogram_count_order = str_c(as.logical(metaprogram_count),
                                                  collapse = " | ")) %>%
        left_join(closures, by = "call_id") %>%
        group_by(function_id, formal_parameter_count,
                 argument_mode_order, expression_type_order,
                 value_type_order, lookup_count_order,
                 metaprogram_count_order, force_order) %>%
        print() %>%
        summarize(function_name = list(unique(function_name)),
                  order_count = n()) %>%
        ungroup()

    list(orders = orders)
}


summarize_analyses  <- function(analyses) {
    orders <-
        analyses$orders %>%
        group_by(function_id, formal_parameter_count,
                 argument_mode_order, expression_type_order,
                 value_type_order, lookup_count_order,
                 metaprogram_count_order, force_order) %>%
        summarize(function_name = str_c(unique(unlist(function_name)),
                                        collapse = " | "),
                  order_count = sum(order_count))

    list(orders = orders)
}

visualize_analyses <- function(analyses) {
    list()
}


latex_analyses <- function(analyses) {
    list()
}

main <- function() {
    analyzer <-
        create_analyzer("Type Order Correlation",
                        c("calls", "arguments"),
                        reduce_analysis,
                        combine_analyses,
                        summarize_analyses,
                        visualize_analyses,
                        latex_analyses)
    drive_analysis(analyzer)
}

main()
summary(warnings())
