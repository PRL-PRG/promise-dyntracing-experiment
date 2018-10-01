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
suppressPackageStartupMessages(library(promisedyntracer))
suppressPackageStartupMessages(library(stringr))
suppressPackageStartupMessages(library(purrr))
suppressPackageStartupMessages(library(shinycssloaders))
suppressPackageStartupMessages(library(future))
plan(multiprocess)
library(promises)

TRACER_SESSION_COUNTER <- 0

TRACE_DIRPATH <- tempdir()

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
         r_dyntrace_binpath = arguments$args[1])
}

import_data_table <- function(session_trace_dirpath, data_filepath) {
    read_data_table(print(path(session_trace_dirpath,
                         str_c(data_filepath, ".csv"))))
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
               wellPanel(
                   column(8),
                   downloadButton("downloadCode",
                                  "Download Code"),
                   actionButton("traceButton",
                                "Trace",
                                icon = icon("play"))),
               aceEditor("codeEditor",
                         "## write R code and press 'Trace' to trace it\n\n",
                         theme = "chrome", mode = "r",
                         readOnly = FALSE, font = 18,
                         autoComplete = "enabled",
                         height = "550px"),
               fileInput("uploadCode",
                         NULL,
                         accept = c("text/plain"),
                         width = "100%"),
               htmlOutput("tracerOutput"))

    right_panel <-
        column(7,
               tabsetPanel(type = "tabs",
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
                                        readOnly = FALSE, font = 18,
                                        autoComplete = "enabled"))))

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

create_tracer_server <- function(r_dyntrace_binpath) {
    function(input, output, session) {

        TRACER_SESSION_COUNTER <<- TRACER_SESSION_COUNTER + 1
        restored <- 0
        traced <- 0

        values <- reactiveValues(
            trace_dirpath = path(TRACE_DIRPATH,
                                 toString(TRACER_SESSION_COUNTER)),
            tracer_output = list(content = "", status = 0, time = NULL))

        observe({
            dir_create(values$trace_dirpath)
            dir_create(compute_function_definition_dirpath(values$trace_dirpath))
            print(values$trace_dirpath)
            print(compute_function_definition_dirpath(values$trace_dirpath))
        })


        output$dataTableViewer <- DT::renderDataTable(data.frame())

        ## clean up the tracing data on session exit
        onSessionEnded(function() {
            cat("Session ended: deleting", values$trace_dirpath,"\n")
            ## dir_delete(values$trace_dirpath)
        })

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
            print(r_dyntrace_binpath)
            tracing_start_time <- Sys.time()
            procout <- system2(r_dyntrace_binpath,
                               stdout = TRUE,
                               stderr = TRUE,
                               args = c("--slave",
                                        str_c("--file=",
                                              code_filepath,
                                              sep = "")))
            tracing_time <- Sys.time() - tracing_start_time
            print("System output is : ")
            print(procout)
            if(is.null(attr(procout, "status"))) attr(procout, "status") <- 0
            list(content = procout,
                 status = attr(procout, "status"),
                 time = tracing_time)
        }

        observeEvent(input$traceButton, {
            if (!restored) {
                showModal(
                    modalDialog(
                        "Please wait while the code is being traced",
                        title = NULL,
                        footer = NULL,
                        size = "l",
                        easyClose = FALSE,
                        fade = TRUE))
                isolate({
                    code_filepath <- path(values$trace_dirpath, "code.R")
                    code <- wrap_code(values$trace_dirpath, input$codeEditor)
                    future({
                        trace_code(code_filepath, code)
                    }) %>%
                        then(
                            onFulfilled = function(value) {
                                list(content = value$content,
                                     status =
                                         file_exists(path(values$trace_dirpath, "ERROR")) |
                                         value$status,
                                     time = value$time)
                            },
                            onRejected = function(err) {
                                list(content = err, status = 1, time = toString(-1))
                            }) %>%
                        then(
                            onFulfilled = function(output) {
                                values$tracer_output <- output
                                removeModal()
                            }
                        )
                })
            }
            else {
                restored <<- 0
                traced <<- 0
            }
        })

        output$tracerOutput <- renderUI(
            div(HTML(str_c(values$tracer_output$content,
                           sep = "", collapse = "<br />")),
                id = if (values$tracer_output$status == 0)
                         "trace-success"
                     else
                         "trace-failure")
        )

        ## observe({
        ##     values$tracer_output
        ##     if(!is.null(values$tracer_output$time)) {
        ##         label <- str_c("Trace [", values$tracer_output$time, "]")
        ##         updateActionButton(session, "traceButton",
        ##                            label = label,
        ##                            icon = icon("play"))
        ##     }
        ## })

        observe({
            print("observe begin")
            values$tracer_output
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

            function_filenames <-
                compute_function_definition_dirpath(values$trace_dirpath) %>%
                dir_ls(type = "file") %>%
                path_file()

            updateSelectizeInput(session,
                                 "functionDefinitionSelector",
                                 choices = function_filenames,
                                 selected = first(function_filenames))
        })


        observeEvent(input$functionDefinitionSelector, {
            function_id <- input$functionDefinitionSelector
            if (nchar(function_id) != 0) {
                updateAceEditor(
                    session,
                    "functionDefinitionEditor",
                    value =
                        import_function_definition(compute_function_definition_dirpath(values$trace_dirpath),
                                                   function_id))
            }
        })

        observeEvent(input$dataTableSelector, {
            data_table_name <- input$dataTableSelector
            if (nchar(data_table_name) != 0) {
                output$dataTableViewer <-
                    DT::renderDataTable(
                            import_data_table(values$trace_dirpath,
                                              data_table_name),
                            options = list(scrollX = TRUE, scrollY = TRUE))
            }
        })

        ## input$file1 will be NULL initially. After the user selects
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
                                        #state$values$ <- vals$sum
        })
                                        # Read values from state$values when we restore
        onRestore(function(state) {
            values$trace_dirpath <- state$values$trace_dirpath
            updateAceEditor(session,
                            "codeEditor",
                            value = state$input$codeEditor)
            values$tracer_output <- state$values$tracer_output
            restored <<- 1
        })
    }
}

main <- function() {

    settings <- parse_program_arguments()

    tracer_app <- shinyApp(ui = create_tracer_ui,
                           server = create_tracer_server(settings$r_dyntrace_binpath),
                           enableBookmarking = "server",
                           onStart = function() {
                               cat("TraceR Started!\n")
                           })

    runApp(tracer_app,
           port = settings$port,
           launch.browser = settings$browser,
           host = settings$host)
}

main()
