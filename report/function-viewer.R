options(error = quote({dump.frames(to.file=FALSE); q();}))

suppressPackageStartupMessages(library(shiny))
suppressPackageStartupMessages(library(shinyAce))
suppressPackageStartupMessages(library(DT))
suppressPackageStartupMessages(library(optparse))
suppressPackageStartupMessages(library(stringr))
suppressPackageStartupMessages(library(fs))
suppressPackageStartupMessages(library(rlang))
suppressPackageStartupMessages(library(readr))
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(styler))
suppressPackageStartupMessages(library(purrr))
suppressPackageStartupMessages(library(tibble))
suppressPackageStartupMessages(library(promisedyntracer))
suppressPackageStartupMessages(library(shinyWidgets))

parse_program_arguments <- function() {
    usage <-
        "%prog summarized-data-dirpath function-definition-dirpath [options]"

    description <-
        str_c("",
              str_c("summarized-data-dirpath      ",
                    "directory containing summarized data"),
              str_c("function-definition-dirpath  ",
                    "directory containing function definitions"),
              sep = "\n")

    option_list <-
        list(make_option(c("--port"),
                         type = "integer",
                         default = getOption("shiny.port", 4000),
                         help = str_c("the TCP port application should",
                                      "listen on [default %default]",
                                      sep = " ")),

             make_option(c("--host"),
                         type = "character",
                         default = getOption("shiny.host", "127.0.0.1"),
                         help = str_c("the IPv4 address application should",
                                      "listen on [default %default]",
                                      sep = " ")),

             make_option(c("--browser"),
                         type = "character",
                         default = getOption("shiny.launch.browser",
                                             interactive()),
                         help = str_c("the web browser to launch after",
                                      "the app is started [default %default]",
                                      sep = " ")))

    option_parser <-
        OptionParser(usage = usage,
                     description = description,
                     add_help_option = TRUE,
                     option_list = option_list)

    arguments <- parse_args2(option_parser)

    list(port = arguments$options$port,
         host = arguments$options$host,
         browser = arguments$options$browser,
         summarized_data_dirpath = arguments$args[1],
         function_definition_dirpath = arguments$args[2])
}

create_function_viewer_app <- function(summarized_data_dirpath,
                                       function_definition_dirpath) {

    read_summarized_data_table <- function(data_filepath) {
        read_data_table(path(summarized_data_dirpath, data_filepath))
    }

    orders <- as.tibble(read_summarized_data_table("type-order-correlation/summary/orders.csv"))

    idChoices <- orders$function_id

    nameChoices <- str_c(seq2_along(1, orders$function_name),
                         ". ",
                         orders$function_name)

    read_function_definition <- function(function_id) {
        ## readChar(file_name, file.info(file_name)$size) %>%
        path(function_definition_dirpath, function_id) %>%
            read_lines() %>%
            style_text() %>%
            str_c(collapse = "\n")
    }

    get_parameter_force_order <- function(required_function_id) {
        order_to_vector <- function(order) {
            c(stringr::str_split(order, " \\| ", simplify = TRUE))
        }

        rows <-
            orders %>%
            filter(function_id == required_function_id) %>%
            select(-function_id) %>%
            mutate(index = row_number()) %>%
            pmap(function(...) {
                args <- list(...)

                rbind(order_to_vector(args$argument_mode_order),
                      order_to_vector(args$expression_type_order),
                      order_to_vector(args$value_type_order),
                      order_to_vector(args$lookup_count_order),
                      order_to_vector(args$metaprogram_count_order)) %>%
                    as.tibble() %>%
                    `colnames<-`(0:(args$formal_parameter_count - 1)) %>%
                    mutate(force_order = str_replace_all(args$force_order, "\\|", "~"),
                           order_count = args$order_count) %>%
                    add_column(Interpretation = c("Argument Mode", "Expression Type",
                                                  "Value Type", "Lookup",
                                                  "Metaprogram"), .before = 1) %>%
                    add_column("Order Number" = toString(args$index), .before = 1) %>%
                    rbind(rep("━━━━", args$formal_parameter_count + 1))
            }) %>%
          bind_rows()
    }

    left_panel <- column(5,
                         wellPanel(
                             fluidRow(
                                 column(6,
                                        selectizeInput("functionId", "Function ID", #width = "50%",
                                                       choices = idChoices, #selected = idChoices[1],
                                                       options = list(placeholder = 'Please select an option below',
                                                       maxOptions = length(idChoices)))),
                                        column(6, selectizeInput("functionName", "Function Name", #width = "50%",
                                                                 choices = nameChoices, #selected = nameChoices[1]),
                                                                 options = list(placeholder = 'Please select an option below',
                                                                      maxOptions = length(nameChoices)))))),
                         aceEditor("functionEditor",
                                   print(read_function_definition(idChoices[1])),
                                   theme = "chrome", mode = "r", readOnly = FALSE,
                                   autoComplete = "disabled", font = 20,
                                   height = "600px"))

    right_panel <- column(7, DT::dataTableOutput("parameterForceOrder"))

    function_viewer_ui <-
        fluidPage(includeCSS("report/custom.css"),
                  titlePanel("Function Explorer"),
                  fluidRow(left_panel, right_panel))

    function_viewer_server <- function(input, output, session) {

        functionDefinition <- reactive({
            str_c("You selected ", input$functionId)
        })

        observeEvent(input$functionId, {
            position <- match(input$functionId, idChoices)
            if(!is.na(position)
               & !is.null(position)
               & position < length(nameChoices)
               & input$functionName != nameChoices[position]) {
                updateAceEditor(session,
                                "functionEditor",
                                value = read_function_definition(input$functionId))
                updateSelectizeInput(session, "functionName", selected = nameChoices[position])
            }
        })

        observeEvent(input$functionName, {
            position <- match(input$functionName, nameChoices)
            if(!is.na(position) & !is.null(position) & position < length(idChoices) & input$functionId != idChoices[position]) {
                previous_position <- position
                updateAceEditor(session,
                                "functionEditor",
                                value = read_function_definition(idChoices[position]))
                updateSelectizeInput(session, "functionId", selected = idChoices[position])
                ## output$parameterForceOrder <-
                ##     DT::renderDataTable(datatable(get_parameter_force_order(input$functionId),
                ##                                   rownames = FALSE,
                ##                                   options = list(pageLength = 20,
                ##                                                  scrollX = TRUE,
                ##                                                  scrollY = TRUE)))
            }
        })

        output$parameterForceOrder <-
            DT::renderDataTable({
                datatable(get_parameter_force_order(input$functionId),
                          rownames = FALSE,
                          filter = "top",
                          options = list(pageLength = 20,
                                         scrollX = TRUE,
                                         scrollY = TRUE))
            })

    }

    shinyApp(ui = function_viewer_ui,
             server = function_viewer_server)

#     output$functionEditor <- renderUI({
#     shinyAce::aceEditor(
#       "functionEditor",
#       theme = "monokai",
#       mode = "r",
#       value = str_c("You selected ", input$functionId),
#       autoComplete = "disabled",
#       font = 18)
#     })
#
    # output$functionEditor = renderPlot({
    #   barplot(WorldPhones[,input$region]*1000,
    #           ylab = "Number of Telephones", xlab = "Year")
    # })
}

main <- function() {
    settings <- parse_program_arguments()

    runApp(create_function_viewer_app(settings$summarized_data_dirpath,
                                      settings$function_definition_dirpath),
           port = settings$port,
           launch.browser = settings$browser,
           host = settings$host)
}

main()
