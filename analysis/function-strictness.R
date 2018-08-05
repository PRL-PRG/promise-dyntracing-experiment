#!/usr/bin/env Rscript

## WARN - no good way in R to find path of current script
##        perhaps the only good solution is to create a
##        package for the analysis ?
source("analysis/utils.R")
source("analysis/analysis.R")

summarize_analyses <- function(analyses) {

    closures <-
        analyses$`functions` %>%
        filter(type == "Closure") %>%
        group_by(id) %>%
        summarize(calls = sum(calls),
                  arguments = first(arguments))

    zero_argument_closures <-
        closures %>%
        filter(arguments == 0)

    all_closure_count <- nrow(closures)

    force_order_distribution <-
        analyses$`parameter-force-order` %>%
        group_by(function_id, order) %>%
        summarize(call_count = sum(count)) %>%
        mutate(order_count = n()) %>%
        arrange(desc(order_count)) %>%
        left_join(closures, by = c("function_id" = "id"))

    parameter_usage_distribution <-
        analyses$`parameter-usage-count` %>%
        left_join(closures, by = c("function_id" = "id")) %>%
        group_by(function_id, position) %>%
        summarize(use = sum(use, unpromised),
                  calls = first(calls)) %>%
        ungroup() %>%
        mutate(classification = ifelse(use == calls, "All",
                                ifelse(use == 0, "Some",
                                       "Mixed")))

    all_position_count <- nrow(parameter_usage_distribution)

    parameter_usage_distribution_summary <-
        parameter_usage_distribution %>%
        group_by(classification) %>%
        summarize(count = n()) %>%
        mutate(relative_count = count / all_position_count)

    function_parameter_usage_distribution <-
        parameter_usage_distribution %>%
        group_by(function_id) %>%
        summarize(classification =
                      if (all(classification == "All")) "All"
                      else if (any(classification == "Some") & any(classification == "Mixed")) "Some + Mixed"
                      else if(any(classification == "Some")) "Some"
                      else if (any(classification == "Mixed")) "Mixed"
                      else "Unknown") %>%
        ungroup() %>%
        add_row(function_id = zero_argument_closures$id,
                classification = "Zero")

    function_parameter_usage_distribution_summary <-
        function_parameter_usage_distribution %>%
        group_by(classification) %>%
        summarize(count = n()) %>%
        mutate(relative_count = count / all_closure_count)
    ## parameter_position_distribution <-
    ##     analyses$parameter_usage_count %>%
    ##     group_by(function_id, position, default, use) %>%


    ## distribution of force order of arguments of all
    # functions which are called more than once.
    ## force_order_distribution <-
    ##     force_order %>%
    ##     group_by(function_id) %>%
    ##     summarize(order_count = n())
    ##     summarize(order_count = length(unique(formal_parameter_position_usage_order))) %>%
    ##     left_join(functions, by = c("function_id" = "id")) %>%
    ##     filter(calls > 1) %>%
    ##     arrange(desc(order_count))


  ##   total_function_count <- length(argument_usage_count$function_id)

  ##   argument_usage_distribution <-
  ##       argument_usage_count %>%
  ##       group_by(order_count) %>%
  ##       summarize(function_count = length(function_id))

  ##   argument_usage_distribution <-
  ##       argument_usage_distribution %>%
  ##       mutate(relative_function_count = function_count / total_function_count)

  ##   call_count <-
  ##       analyses$`function-name` %>%
  ##       group_by(function_id) %>%
  ##       summarize(call_count = sum(as.numeric(count)))

  ##   parameter_usage_distribution <-
  ##       analyses$`function-formal-parameter-usage-count` %>%
  ##       group_by(function_id, formal_parameter_position) %>%
  ##       summarize(evaluation_count = sum(count)) %>%
  ##       ungroup() %>%
  ##       left_join(call_count, by = "function_id")

  ##   parameter_classification <-
  ##       parameter_usage_distribution %>%
  ##       group_by(function_id, formal_parameter_position) %>%
  ##       summarize(classification = if(evaluation_count >= call_count)
  ##                                      "Always Evaluated"
  ##                                  else if(evaluation_count == 0)
  ##                                      "Never Evaluated"
  ##                                  else
  ##                                      "Sometimes Evaluated") %>%
  ##   ungroup() %>%
  ##   group_by(classification) %>%
  ##   summarize(parameter_count = sum(as.numeric(n())))

  ##   total_parameter_count <- sum(parameter_classification$parameter_count)

  ## parameter_classification <-
  ##     parameter_classification %>%
  ##   mutate(relative_parameter_count =
  ##            parameter_count / total_parameter_count)

  ##   function_classification <-
  ##     parameter_usage_distribution %>%
  ##     group_by(function_id) %>%
  ##     summarize(classification = if(all(evaluation_count >= call_count))
  ##                                    "All"
  ##                                else if(any(evaluation_count == 0))
  ##                                    "Some"
  ##                                else
  ##                                    "Mixed") %>%
  ##     ungroup()

  ##   some_function_count <-
  ##       function_classification %>%
  ##       filter(classification == "Some")

  ##   some_function_count <- length(some_function_count$function_id)

  ##   some_function_call_distribution <-
  ##       function_classification %>%
  ##       filter(classification == "Some") %>%
  ##       left_join(call_count, by = "function_id") %>%
  ##       select(call_count)

  ##   some_function_rest <-
  ##       some_function_call_distribution %>%
  ##       filter(call_count > 20)

  ##   some_function_call_distribution <-
  ##       some_function_call_distribution %>%
  ##       filter(call_count <= 20) %>%
  ##       group_by(call_count) %>%
  ##       summarize(function_count = n()) %>%
  ##       ungroup() %>%
  ##       add_row(call_count = "> 20", function_count = nrow(some_function_rest))

  ##   all_function_count <-
  ##       function_classification %>%
  ##       filter(classification == "All")

  ##   all_function_count <- length(all_function_count$function_id)

  ##   all_function_call_distribution <-
  ##       function_classification %>%
  ##       filter(classification == "All") %>%
  ##       left_join(call_count, by = "function_id") %>%
  ##       select(call_count)

  ##   all_function_rest <-
  ##       all_function_call_distribution %>%
  ##       filter(call_count > 20)

  ##   all_function_call_distribution <-
  ##       all_function_call_distribution %>%
  ##       filter(call_count <= 20) %>%
  ##       group_by(call_count) %>%
  ##       summarize(function_count = n()) %>%
  ##       ungroup() %>%
  ##       add_row(call_count = "> 20", function_count = nrow(all_function_rest))

  ##   mixed_function_count <-
  ##       function_classification %>%
  ##       filter(classification == "Mixed")

  ##   mixed_function_count <- length(mixed_function_count$function_id)

  ##   mixed_function_call_distribution <-
  ##       function_classification %>%
  ##       filter(classification == "Mixed") %>%
  ##       left_join(call_count, by = "function_id") %>%
  ##       select(call_count)

  ##   mixed_function_rest <-
  ##       mixed_function_call_distribution %>%
  ##       filter(call_count > 20)

  ##   mixed_function_call_distribution <-
  ##       mixed_function_call_distribution %>%
  ##       filter(call_count <= 20) %>%
  ##       group_by(call_count) %>%
  ##       summarize(function_count = n()) %>%
  ##       ungroup() %>%
  ##       add_row(call_count = "> 20", function_count = nrow(mixed_function_rest))

  ##   function_classification <-
  ##       function_classification %>%
  ##       group_by(classification) %>%
  ##       summarize(function_count = sum(as.numeric(n()))) %>%
  ##       mutate(relative_function_count = function_count / total_function_count)

  ##   list(argument_usage_count = argument_usage_count,
  ##        argument_usage_distribution = argument_usage_distribution,
  ##        function_classification = function_classification,
  ##        some_function_call_distribution = some_function_call_distribution,
  ##        all_function_call_distribution = all_function_call_distribution,
  ##        mixed_function_call_distribution = mixed_function_call_distribution,
  ##        parameter_classification = parameter_classification,
  ##        )
    list(force_order_distribution = force_order_distribution,
         function_parameter_usage_distribution = function_parameter_usage_distribution,
         function_parameter_usage_distribution_summary = function_parameter_usage_distribution_summary,
         parameter_usage_distribution = parameter_usage_distribution,
         parameter_usage_distribution_summary = parameter_usage_distribution_summary,
         summary = tibble(all_closure_count = all_closure_count,
                          all_position_count = all_position_count))
                          ##  mixed_function_count = mixed_function_count,
                          ##  all_function_count = all_function_count,
                          ##  some_function_count = some_function_count,
                          ## total_parameter_count = total_parameter_count))

}

visualize_analyses <- function(analyses) {

    all_closure_count <- analyses$summary$all_closure_count
    all_position_count <- analyses$summary$all_position_count
    ## total_function_count <- analyses$summary$total_function_count
    ## total_parameter_count <- analyses$summary$total_parameter_count
    ## mixed_function_count <- analyses$summary$mixed_function_count
    ## all_function_count <- analyses$summary$all_function_count
    ## some_function_count <- analyses$summary$some_function_count

  force_order_distribution <-
      analyses$force_order_distribution %>%
      filter(calls > 1) %>%
      group_by(order_count) %>%
      summarize(function_count = n()) %>%
      rename("Argument Evaluation Orders" = order_count) %>%
      ggplot(aes(`Argument Evaluation Orders`, function_count)) +
    geom_col() +
    ## scale_y_continuous(sec.axis = sec_axis(~ . / total_function_count,
    ##                                        labels = relative_labels),
    ##                    labels = count_labels) +
    labs(y = "Function count",
         title =  "Function count per argument evaluation order") +
    scale_fill_gdocs()
    #scale_x_continuous(limits = c(0, max(analyses$force_order_distribution$order_count)))



  parameter_usage_distribution <-
      analyses$parameter_usage_distribution_summary %>%
      rename("Formal parameter classification" = classification) %>%
      ggplot(aes(`Formal parameter classification`, relative_count)) +
      geom_col() +
      scale_y_continuous(sec.axis = sec_axis(~ . * all_position_count,
                                             labels = count_labels),
                         labels = relative_labels) +
      labs(y = "Formal parameter count (%)",
           title =  "Formal parameter distribution by evaluation mode") +
      scale_fill_gdocs()

    function_parameter_usage_distribution<-
        analyses$function_parameter_usage_distribution_summary %>%
        rename("Function classification" = classification) %>%
        ggplot(aes(`Function classification`, relative_count)) +
        geom_col() +
        scale_y_continuous(sec.axis = sec_axis(~ . * all_closure_count,
                                               labels = count_labels),
                           labels = relative_labels) +
        labs(y = "Function count (%)",
             title =  "Function distribution by evaluation mode") +
        scale_fill_gdocs()


  ##   some_function_call_distribution <-
  ##       analyses$some_function_call_distribution %>%
  ##       rename("Calls" = call_count) %>%
  ##       ggplot(aes(`Calls`, function_count)) +
  ##       geom_col() +
  ##       scale_x_discrete(limits = c(1:20, "> 20")) +
  ##       scale_y_continuous(sec.axis = sec_axis(~ . / some_function_count,
  ##                                              labels = relative_labels),
  ##                          labels = count_labels) +
  ##       labs(y = "Function count",
  ##            title =  "Function count per number of calls for Some functions") +
  ##       scale_fill_gdocs()

  ##   all_function_call_distribution <-
  ##       analyses$all_function_call_distribution %>%
  ##       rename("Calls" = call_count) %>%
  ##       ggplot(aes(`Calls`, function_count)) +
  ##       geom_col() +
  ##       scale_x_discrete(limits = c(1:20, "> 20")) +
  ##       scale_y_continuous(sec.axis = sec_axis(~ . / all_function_count,
  ##                                              labels = relative_labels),
  ##                          labels = count_labels) +
  ##       labs(y = "Function count",
  ##            title =  "Function count per number of calls for All functions") +
  ##       scale_fill_gdocs()


  ##   mixed_function_call_distribution <-
  ##       analyses$mixed_function_call_distribution %>%
  ##       rename("Calls" = call_count) %>%
  ##       ggplot(aes(`Calls`, function_count)) +
  ##       geom_col() +
  ##       scale_x_discrete(limits = c(1:20, "> 20")) +
  ##       scale_y_continuous(sec.axis = sec_axis(~ . / mixed_function_count,
  ##                                              labels = relative_labels),
  ##                          labels = count_labels) +
  ##       labs(y = "Function count",
  ##            title =  "Function count per number of calls for Mixed functions") +
  ##       scale_fill_gdocs()


    list(force_order_distribution = force_order_distribution,
         parameter_usage_distribution = parameter_usage_distribution,
         function_parameter_usage_distribution = function_parameter_usage_distribution)
    ## list(function_strictness_ordering = function_strictness_ordering,
    ##      parameter_classification = parameter_classification,
    ##      function_classification = function_classification,
    ##      some_function_call_distribution = some_function_call_distribution,
    ##      all_function_call_distribution = all_function_call_distribution,
    ##      mixed_function_call_distribution = mixed_function_call_distribution)
}

latex_analyses <- function(analyses) {
    list()
}

main <- function() {
    analyzer <-
      create_analyzer("Function Strictness Analysis",
                      combine_analyses,
                      summarize_analyses,
                      visualize_analyses,
                      latex_analyses)
    drive_analysis(analyzer)
}

main()
warnings()
