suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(promisedyntracer))
suppressPackageStartupMessages(library(styler))
suppressPackageStartupMessages(library(optparse))
suppressPackageStartupMessages(library(stringr))

show_function_definition <- function(settings) {

    function_definition <-
        read_data_table(settings$filepath,
                        binary = settings$binary,
                        compression_level = settings$compression_level) %>%
        filter(function_id == settings$function_id) %>%
        top_n(1)

    separator <- str_c(str_c(rep("=", 80), collapse = ""), "\n")

    cat(separator)

    cat("NAME  : ", function_definition$function_name, "\n")

    cat("ID    : ", function_definition$function_id, "\n")

    cat("SOURCE: ", function_definition$script, "\n")

    cat(separator)

    styled_definition <-
        style_text(function_definition$definition,
                   reindention = tidyverse_reindention())

    print(styled_definition, "\n")

    cat(separator)
}

parse_program_arguments <- function() {

    usage <- "%prog function-definitions-filepath %options"

    description <- paste(
        "function-definitions-filepath   file containing function definitions",
        "function-id                     function id",
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

    list(filepath = arguments$args[1],
         function_id = arguments$args[2],
         binary = arguments$options$binary,
         compression_level = as.integer(arguments$options$compression_level))
}


main <- function(settings) {
    settings <- parse_program_arguments()

    show_function_definition(settings)
}


main()
