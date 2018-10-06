options(error = quote({dump.frames(to.file=FALSE); q();}))
options(shiny.fullstacktrace = TRUE)
options(shiny.error = recover)

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
suppressPackageStartupMessages(library(promisedyntracer))
suppressPackageStartupMessages(library(stringr))
suppressPackageStartupMessages(library(purrr))
suppressPackageStartupMessages(library(lubridate))
suppressPackageStartupMessages(library(shinycssloaders))
suppressPackageStartupMessages(library(future))
plan(multiprocess)
library(promises)

TRACER_SESSION_COUNTER <- 0

parse_program_arguments <- function() {
    usage <-
        "%prog r-dyntrace-binpath [options]"

    description <-
        str_c("",
              "r-dyntrace-binpath   path to the R-Dyntrace binary",
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
         r_dyntrace_binpath = arguments$args[1],
         tracer_output_dirpath = arguments$args[2])
}

check_program_arguments <- function(settings) {

    if (!file_exists(settings$r_dyntrace_binpath)) {
        stop(str_glue("R executable '{settings$r_dyntrace_binpath}'",
                      "does not exist. \n",
                      "Please provide a valid R executable path.",
                      .sep = "\n"))
    }
}

import_data_table <- function(session_trace_dirpath, data_filepath) {
    read_data_table(path(session_trace_dirpath,
                         str_c(data_filepath, ".csv")))
}

import_function_definition <- function(function_definition_dirpath,
                                       function_id) {
    path(function_definition_dirpath, function_id) %>%
        read_lines() %>%
        style_text() %>%
        str_c(collapse = "\n")
}

wrap_code <- function(session_trace_dirpath, code) {
            str_glue(
                "setwd('{session_trace_dirpath}')",
                "library(promisedyntracer)",
                "invisible(dyntrace_promises(",
                "    {{",
                "{code}",
                "    }},",
                "    '{session_trace_dirpath}/code.trace',",
                "    '{session_trace_dirpath}',",
                "    verbose = FALSE,",
                "    enable_trace = FALSE,",
                "    truncate = TRUE,",
                "    binary = FALSE,",
                "    compression_level = 0,",
                "    analysis_switch =",
                "        list2env(list(enable_metadata_analysis = TRUE,",
                "                      enable_object_count_size_analysis = TRUE,",
                "                      enable_function_analysis = TRUE,",
                "                      enable_promise_type_analysis = TRUE,",
                "                      enable_promise_slot_mutation_analysis = TRUE,",
                "                      enable_promise_evaluation_analysis = TRUE,",
                "                      enable_strictness_analysis = TRUE,",
                "                      enable_side_effect_analysis = TRUE))))",
                .sep = "\n")
}

create_tracer_ui <- function(request) {

    left_panel <-
        column(5,
               aceEditor("codeEditor",
                         "## write R code and press 'Trace' to trace it\n\n",
                         theme = "chrome", mode = "r",
                         readOnly = FALSE, font = 20,
                         autoComplete = "enabled",
                         height = "600px"),
               wellPanel(
                   downloadButton("downloadCode",
                                  "Download Code"),
                   actionButton("traceButton",
                                "Trace",
                                icon = icon("play"))),
               fileInput("uploadCode",
                         NULL,
                         accept = c("text/plain"),
                         width = "100%"))

    right_panel <-
        column(7,
               tabsetPanel(type = "tabs",
                           tabPanel("Tracer Output",
                                    icon = icon("terminal"),
                                    htmlOutput("tracerOutput")),
                           tabPanel("Environment Variables",
                                    icon = icon("tree"),
                                    aceEditor(
                                        "environmentVariableEditor",
                                        "",
                                        theme = "chrome", mode = "shell",
                                        readOnly = TRUE, font = 20,
                                        autoComplete = "enabled",
                                        height = "650px")),
                           tabPanel("Tracer Configuration",
                                    icon = icon("cogs"),
                                    aceEditor(
                                        "tracerConfigurationEditor",
                                        "",
                                        theme = "chrome", mode = "text",
                                        readOnly = TRUE, font = 20,
                                        autoComplete = "enabled",
                                        height = "650px")),
                           tabPanel("Data Tables",
                                    icon = icon("table"),
                                    wellPanel(
                                        column(5,
                                               selectizeInput(
                                                   "dataTableSelector",
                                                   label = NULL,
                                                   choices = NULL)),
                                        downloadButton("downloadDataTable",
                                                       "Download Data Table")),
                                    withSpinner(DT::dataTableOutput("dataTableViewer"))),
                           tabPanel("Function Definitions",
                                    icon = icon("code"),
                                    wellPanel(
                                        column(5,
                                               selectizeInput(
                                                   "functionDefinitionSelector",
                                                   label = NULL,
                                                   choices = NULL)),
                                        downloadButton(
                                            "downloadFunctionDefinition",
                                            "Download Function Definition")),
                                    aceEditor(
                                        "functionDefinitionEditor",
                                        "",
                                        theme = "chrome", mode = "r",
                                        readOnly = FALSE, font = 20,
                                        autoComplete = "enabled",
                                        height = "600px"))))

    tracer_ui <- fluidPage(includeCSS("scripts/tracer.css"),
                           title = "TraceR",
                           titlePanel("TraceR"),
                           fluidRow(left_panel, right_panel))
}

# https://shinydata.wordpress.com/2015/02/02/a-few-things-i-learned-about-shiny-and-reactive-programming/
# TODO - remove the session filepath and replace with code run filepath
#        check if the restored logic can be cleaned
#        the tracer should not run on the first load, try to correct that


compute_function_definition_dirpath <- function(trace_dirpath) {
    path(trace_dirpath, "functions")
}

create_tracer_server <- function(r_dyntrace_binpath, tracer_output_dirpath) {
    function(input, output, session) {

        values <- reactiveValues(
            trace_dirpath = NULL,
            tracer_output = list(content = "",
                                 status = 0,
                                 time = as.period(Sys.time() %--% Sys.time())))

        output$downloadCode <- downloadHandler(
            filename = function() {
                str_c("tracer-input-",
                      format(Sys.time(), '%Y-%m-%d-%H-%M-%S'),
                      ".R",
                      sep = "")
            },
            content = function(connection) {
                writeChar(input$codeEditor, connection, eos = NULL)
            }
        )

        output$downloadDataTable <- downloadHandler(
            filename = function() {
                data_table_name <- input$dataTableSelector
                if (nchar(data_table_name) == 0)
                    data_table_name <- "empty"

                str_c("tracer-output-",
                      data_table_name,
                      "-",
                      format(Sys.time(), '%Y-%m-%d-%H-%M-%S'),
                      ".csv",
                      sep = "")
            },

            content = function(connection) {
                if (nchar(input$dataTableSelector) != 0) {
                    write_csv(import_data_table(values$trace_dirpath,
                                                input$dataTableSelector),
                              connection)
                } else {
                    writeChar("Empty", connection, eos = NULL)
                }
            }
        )

        output$downloadFunctionDefinition <- downloadHandler(
            filename = function() {
                function_id <- input$functionDefinitionSelector

                if (nchar(function_id) == 0)
                    function_id <- "empty"
                str_c("tracer-output-",
                      function_id,
                      "-",
                      format(Sys.time(), "%Y-%m-%d-%H-%M-%S"),
                      ".R",
                      sep = "")
            },

            content = function(connection) {
                function_definition <- input$functionDefinitionEditor
                if (nchar(function_definition) == 0)
                    function_definition <- "Empty"
                writeChar(function_definition, connection, eos = NULL)
            }
        )

        trace_code <- function(code_filepath, code) {
            writeChar(code, code_filepath)
            tracing_start_time <- Sys.time()
            procout <- system2(r_dyntrace_binpath,
                               stdout = TRUE,
                               stderr = TRUE,
                               args = c("--slave",
                                        str_c("--file=",
                                              code_filepath,
                                              sep = "")),
                               env = c("R_COMPILE_PKGS=0",
                                       "R_DISABLE_BYTECODE=1",
                                       "R_ENABLE_JIT=0",
                                       "R_KEEP_PKG_SOURCE=yes"))
            if (is.null(attr(procout, "status")))
                attr(procout, "status") <- 0
            list(content = procout,
                 status = attr(procout, "status"),
                 time = as.period(tracing_start_time %--% Sys.time()))
        }


        ## This code is observer and not reactive because
        ## the tracer output is assigned asynchronously by
        ## the promise resolution chain.
        observeEvent(input$traceButton, {
            showModal(
                modalDialog(
                    "Please wait while the code is being traced",
                    title = NULL,
                    footer = NULL,
                    size = "l",
                    easyClose = FALSE,
                    fade = TRUE))

            TRACER_SESSION_COUNTER <<- TRACER_SESSION_COUNTER + 1
            trace_dirpath <- path(tracer_output_dirpath,
                                  toString(TRACER_SESSION_COUNTER))
            dir_create(trace_dirpath)
            dir_create(compute_function_definition_dirpath(trace_dirpath))

            code_filepath <- path(trace_dirpath, "code.R")
            code <- wrap_code(trace_dirpath, input$codeEditor)
            future({
                trace_code(code_filepath, code)
            }) %>%
                then(
                    onFulfilled = function(value) {
                        list(content = value$content,
                             status =
                                 file_exists(path(trace_dirpath,
                                                  "ERROR")) |
                                 value$status,
                             time = value$time)
                    },
                    onRejected = function(err) {
                        list(content = err,
                             status = 1,
                             time = as.period(Sys.time() %--% Sys.time()))
                    }) %>%
                then(
                    onFulfilled = function(output) {
                        values$trace_dirpath <- trace_dirpath
                        values$tracer_output <- output
                        removeModal()
                    })
        },
        ignoreNULL = TRUE,
        ignoreInit = TRUE)

        output$tracerOutput <- renderUI({
            div(HTML(str_c(values$tracer_output$content,
                           sep = "", collapse = "<br />")),
                class = "tracer-output",
                id = if (values$tracer_output$status == 0)
                         "trace-success"
                     else
                         "trace-failure")
        })

        observeEvent(values$tracer_output, {
            tracing_time <-
                str_c(str_c(round(hour(values$tracer_output$time), 2),
                            "H",
                            sep = ""),
                      str_c(round(minute(values$tracer_output$time), 2),
                            "M",
                            sep = ""),
                      str_c(round(second(values$tracer_output$time), 2),
                            "S",
                            sep = ""),
                      sep = " ")
            updateActionButton(session,
                               "traceButton",
                               label = str_c("Trace [", tracing_time, "]"))
        })

        observeEvent(values$trace_dirpath, {

            req(dir_exists(values$trace_dirpath))

            data_filepaths <-
                values$trace_dirpath %>%
                dir_ls(type = "file") %>%
                keep(function(path) path_ext(path) == "csv") %>%
                path_file() %>%
                path_ext_remove()

            updateSelectizeInput(session,
                                 "dataTableSelector",
                                 choices = data_filepaths,
                                 selected = first(data_filepaths))

            req(dir_exists(
                compute_function_definition_dirpath(values$trace_dirpath)))

            function_filenames <-
                compute_function_definition_dirpath(values$trace_dirpath) %>%
                dir_ls(type = "file") %>%
                path_file()
9
            updateSelectizeInput(session,
                                 "functionDefinitionSelector",
                                 choices = function_filenames,
                                 selected = first(function_filenames))

            configuration_filepath <- path(values$trace_dirpath, "CONFIGURATION")
            req(file_exists(configuration_filepath))
            updateAceEditor(session,
                            "tracerConfigurationEditor",
                            value = read_file(configuration_filepath))

            envvar_filepath <- path(values$trace_dirpath, "ENVVAR")
            req(file_exists(envvar_filepath))
            updateAceEditor(session,
                            "environmentVariableEditor",
                            value = read_file(envvar_filepath))
        })

        observeEvent(input$functionDefinitionSelector, {
            updateAceEditor(
                session,
                "functionDefinitionEditor",
                value =
                    import_function_definition(
                        compute_function_definition_dirpath(req(values$trace_dirpath)),
                        req(input$functionDefinitionSelector)))
        },
        ignoreNULL = TRUE,
        ignoreInit = TRUE)

        output$dataTableViewer <- renderDT({
            datatable(import_data_table(req(values$trace_dirpath),
                                        req(input$dataTableSelector)),
                      rownames = FALSE,
                      options = list(pageLength = 19,
                                     scrollX = TRUE,
                                     scrollY = TRUE))
        })

        ## input$uploadCode will be NULL initially. After the user selects
        ## and uploads a file, it will be a data frame with 'name',
        ## 'size', 'type', and 'datapath' columns. The 'datapath'
        ## column will contain the local filenames where the data can
        ## be found.
        observeEvent(input$uploadCode, {
            updateAceEditor(session,
                            "codeEditor",
                            value = readChar(input$uploadCode$datapath,
                                             input$uploadCode$size))
        })

        observe({
            ## Trigger this observer every time an input changes
            reactiveValuesToList(input)
            session$doBookmark()
        })

        setBookmarkExclude("traceButton")

        onBookmark(function(state) {
            state$values$trace_dirpath <- values$trace_dirpath
            state$values$tracer_output <- values$tracer_output
        })

        onBookmarked(function(url) {
            updateQueryString(url)
        })

        onRestore(function(state) {
            values$trace_dirpath <- state$values$trace_dirpath
            values$tracer_output <- state$values$tracer_output
            updateAceEditor(session,
                            "codeEditor",
                            value = state$input$codeEditor)
        })
    }
}

main <- function() {

    settings <- parse_program_arguments()

    print(settings)

    check_program_arguments(settings)

    dir_create(settings$tracer_output_dirpath)

    tracer_app <- shinyApp(ui = create_tracer_ui,
                           server = create_tracer_server(
                               settings$r_dyntrace_binpath,
                               settings$tracer_output_dirpath
                           ),
                           enableBookmarking = "server",
                           onStart = function() {
                               cat("TraceR Started!\n")
                               dir_create(settings$tracer_output_dirpath)
                               TRACER_SESSION_COUNTER <<-
                                   length(dir_ls(settings$tracer_output_dirpath))
                           })

    runApp(tracer_app,
           port = settings$port,
           launch.browser = settings$browser,
           host = settings$host)
}

main()
