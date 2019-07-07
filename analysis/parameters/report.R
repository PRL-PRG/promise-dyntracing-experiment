suppressPackageStartupMessages(library(rmarkdown))
suppressPackageStartupMessages(library(fs))
suppressPackageStartupMessages(library(optparse))
suppressPackageStartupMessages(library(DT))
suppressPackageStartupMessages(library(fs))
suppressPackageStartupMessages(library(png))
suppressPackageStartupMessages(library(stringr))
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(magrittr))
suppressPackageStartupMessages(library(rlang))
suppressPackageStartupMessages(library(knitr))
suppressPackageStartupMessages(library(png))
suppressPackageStartupMessages(library(grid))
suppressPackageStartupMessages(library(gridExtra))
suppressPackageStartupMessages(library(promisedyntracer))

create_rendering_environment <- function(settings) {

    rendering_environment <- new.env(hash = TRUE)

    evalq({
        summarized_data_dirpath <- path_abs(settings$summarized_data_dirpath)
        visualized_data_dirpath <- path_abs(settings$visualized_data_dirpath)
        binary <- settings$binary
        compression_level <- settings$compression_level

        show_table <- function(tablename) {
            path(summarized_data_dirpath, tablename) %>%
                read_data_table(binary = binary,
                                compression_level = compression_level) %>%
                datatable(fillContainer = TRUE, filter = "top")
        }


        show_graph <- function(graphname) {
            path(visualized_data_dirpath, graphname, ext = "png") %>%
                include_graphics()
        }


        show_two_graphs <- function(first_graphname, second_graphname) {
            load_graph <- function(graphname) {
                grid::rasterGrob(as.raster(readPNG(path(visualized_data_dirpath,
                                                        graphname,
                                                        ext = "png"))),
                                 interpolate = TRUE)
            }
            grid.arrange(load_graph(first_graphname),
                         load_graph(second_graphname),
                         ncol = 2)
        }
    },
    envir = rendering_environment)

    rendering_environment
}


report_analysis_data <- function(settings) {
    dir_create(path_dir(settings$report_output_filepath))
    dir_create(settings$visualized_data_dirpath)
    dir_create(path_dir(settings$latex_macro_filepath))

    if(file_exists(settings$latex_macro_filepath)) {
        file_delete(settings$latex_macro_filepath)
    }

    file_create(settings$latex_macro_filepath)

    ## force path computation to ensure that they are resolved
    ## with respect to the the current working directory.
    ## if passed unevaluated to render function, they are
    ## resolved with respect to the directory that contains
    ## the report.
    input <- path_abs(settings$report_template_filepath)
    output <- path_abs(settings$report_output_filepath)

    ##rendering_environment <- create_rendering_environment(settings)

    rmarkdown::render(input = input,
                      output_file = output,
                      runtime = "auto",
                      params = list(summarized_data_dirpath = settings$summarized_data_dirpath,
                                    visualized_data_dirpath = settings$visualized_data_dirpath,
                                    latex_macro_filepath = settings$latex_macro_filepath))
}


parse_program_arguments <- function() {

    usage <- "%prog report-template-filepath report-output-filepath summarized-data-dirpath visualized-data-dirpath"

    description <- paste(
        "report-template-filepath   path to the input report template file (Rmd)",
        "report-output-filepath     path to the output report file (html/pdf)",
        "summarized-data-dirpath    directory containing summarized data files",
        "visualized-data-dirpath    directory containing visualized data",
        "latex-macro-filepath       file containing latex macros",
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

    list(report_template_filepath = path_abs(arguments$args[1]),
         report_output_filepath = path_abs(arguments$args[2]),
         summarized_data_dirpath = path_abs(arguments$args[3]),
         visualized_data_dirpath = path_abs(arguments$args[4]),
         latex_macro_filepath = path_abs(arguments$args[5]),
         binary = arguments$options$binary,
         compression_level = arguments$options$compression_level)
}


main <- function() {
    settings <- parse_program_arguments()
    print(settings)
    report_analysis_data(settings)
}


main()
