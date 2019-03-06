library(rmarkdown)
library(fs)
library(optparse)

report_analysis_data <- function(settings) {
    dir_create(path_dir(settings$report_output_filepath))

    ## force path computation to ensure that they are resolved
    ## with respect to the the current working directory.
    ## if passed unevaluated to render function, they are
    ## resolved with respect to the directory that contains
    ## the report.
    input <- path_abs(settings$report_template_filepath)
    output <- path_abs(settings$report_output_filepath)

    summarized_data_dirpath <- path_abs(settings$summarized_data_dirpath)
    visualized_data_dirpath <- path_abs(settings$visualized_data_dirpath)

    rmarkdown::render(input = input,
                      output_file = output,
                      runtime = "auto",
                      params = list(summarized_data_dirpath = summarized_data_dirpath,
                                    visualized_data_dirpath = visualized_data_dirpath,
                                    binary = settings$binary,
                                    compression_level = settings$compression_level))
}


parse_program_arguments <- function() {

    usage <- "%prog report-template-filepath report-output-filepath summarized-data-dirpath visualized-data-dirpath"

    description <- paste(
        "report-template-filepath   path to the input report template file (Rmd)",
        "report-output-filepath     path to the output report file (html/pdf)",
        "summarized-data-dirpath    directory containing summarized data files",
        "visualized-data-dirpath    directory containing visualized data",
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

    list(report_template_filepath = arguments$args[1],
         report_output_filepath = arguments$args[2],
         summarized_data_dirpath = arguments$args[3],
         visualized_data_dirpath = arguments$args[4],
         binary = arguments$options$binary,
         compression_level = arguments$options$compression_level)
}


main <- function() {
    settings <- parse_program_arguments()
    print(settings)
    report_analysis_data(settings)
}


main()
