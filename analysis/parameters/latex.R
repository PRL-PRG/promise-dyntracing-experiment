suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(tidyr))
suppressPackageStartupMessages(library(promisedyntracer))
suppressPackageStartupMessages(library(styler))
suppressPackageStartupMessages(library(optparse))
suppressPackageStartupMessages(library(stringr))
suppressPackageStartupMessages(library(readr))
suppressPackageStartupMessages(library(fs))
suppressPackageStartupMessages(library(purrr))
suppressPackageStartupMessages(library(tibble))
source("analysis/utils.R")

options(error = quote({dump.frames(to.file=FALSE); q();}))

## this ensures that all the columns of the tibble are printed
## irrespective of terminal width
options(tibble.width = Inf)


info <- function(...) cat((paste0(...)))


format_as_percentage <- function(x, digits = 1) {

    rounded <- round(100 * x, digits = digits)

    first_decimal_digit = as.integer(rounded * 10) %% 10

    str_c(format(100 * x,
                 digits = digits,
                 nsmall = first_decimal_digit != 0,
                 scientific = FALSE), "%")
}


format_as_absolute <- function(x, digits = 1) {
    count_labels(x, digits)
}

format_as_size <- function(x, digits = 1) {
    memory_size_labels(x, digits)
}


generate_corpus_latex_variables <- function(corpus_data, prefix) {

    corpus_data %>%
        print() %>%
        print() %>%
        pmap(function(script_type, language, script_count,
                      package_count, code, blank, comment) {
            n1 <- str_c("Count", prefix, script_type, language, "Packages")
            n2 <- str_c("Count", prefix, script_type, language, "Scripts")
            n3 <- str_c("Count", prefix, script_type, language, "Code")
            setNames(list(package_count, script_count, code), c(n1, n2, n3))
         }) %>%
        unlist() %>%
        as.list()
}


generate_data_latex_variables <- function(traced_data) {
    traced_data %>%
        print() %>%
        pmap(function(script_type, filename, size) {
            name <- str_c("Size", script_type, str_replace(filename, " ", ""))
            setNames(list(size), c(name))
        }) %>%
        unlist() %>%
        as.list()
}


summarize_traced_data <- function(traced_data) {
    summarized_traced_data <-
        traced_data %>%
        group_by(script_type) %>%
        summarize_if(is.numeric, sum) %>%
        ungroup()


    total_summarized_traced_data <-
        summarized_traced_data %>%
        summarize_if(is.numeric, sum) %>%
        add_column(script_type = "Total", .before = 1)


    bind_rows(summarized_traced_data,
              total_summarized_traced_data) %>%
        gather(filename, size, -script_type) %>%
        mutate(size = as.double(size)) %>%
        mutate(filename = path_ext_remove(filename)) %>%
        mutate(filename = str_to_title(str_replace(filename, "_", " ")))
 }


corpus <- function(analyses) {
    c(generate_corpus_latex_variables(analyses$summarized_installed_corpus, "Installed"),
      generate_corpus_latex_variables(analyses$summarized_traced_corpus, "Traced"),
      generate_data_latex_variables(summarize_traced_data(analyses$traced_data)))
}


objects <- function(analyses) {
    extract_by_type <- function(required_type) {
        analyses$object_count_by_type %>%
            filter(type == required_type) %>%
            pull(relative_count)
    }

    PercObjTypeOther <-
        analyses$object_count_by_type %>%
        filter(!(type %in% c("Pairlist", "Promise", "Environment", "Character",
                             "Integer", "Logical", "Double"))) %>%
        pull(relative_count) %>%
        sum()

    list(PercObjTypePairlist = extract_by_type("Pairlist"),
         PercObjTypeProm = extract_by_type("Promise"),
         PercObjTypeEnv = extract_by_type("Environment"),
         PercObjTypeChar = extract_by_type("Character"),
         PercObjTypeInt = extract_by_type("Integer"),
         PercObjTypeLgl = extract_by_type("Logical"),
         PercObjTypeDbl = extract_by_type("Double"),
         PercObjTypeOther = PercObjTypeOther)
}


functions <- function(analyses) {

    CountFunsClos <-
        analyses$function_count_by_type %>%
        filter(function_type == "Closure") %>%
        pull(function_count)

    list(CountFunsClos = CountFunsClos)
}


arguments <- function(analyses) {

    CountPromArg <-
        analyses$argument_count_by_type %>%
        filter(argument_type == "Promise") %>%
        pull(argument_count)

    list(CountPromArg = CountPromArg)
}


parameters <- function(analyses) {
    list()
}


promises <- function(analyses) {
    list()
}


escaped_arguments <- function(analyses) {

    CountEscArg <-
        analyses$escaped_arguments %>%
        pull(call_count) %>%
        sum()

    PercEscArgNonDef <-
        analyses$escaped_argument_count_by_nature %>%
        filter(argument_nature == "Non Default") %>%
        pull(relative_argument_count)

    PercEscArgDef <-
        analyses$escaped_argument_count_by_nature %>%
        filter(argument_nature == "Default") %>%
        pull(relative_argument_count)

    PercEscArgCallClos <-
        analyses$escaped_argument_function_call_count_by_return_value_type %>%
        filter(return_value_type == "Closure") %>%
        pull(relative_call_count)

    CountEscArgClos <-
        analyses$escaped_arguments %>%
        pull(function_id) %>%
        unique() %>%
        length()

    PercEscArgFunRetValTypeClos <-
        analyses$escaped_argument_function_call_count_by_return_value_type %>%
        filter(return_value_type == "Closure") %>%
        pull(relative_call_count)

    list(CountEscArg = CountEscArg,
         PercEscArgNonDef = PercEscArgNonDef,
         PercEscArgDef = PercEscArgDef,
         PercEscArgCallClos = PercEscArgCallClos,
         CountEscArgClos = CountEscArgClos,
         PercEscArgFunRetValTypeClos = PercEscArgFunRetValTypeClos)
}


latex <- function(analyses, settings) {

    to_latex_command <- function(value, name) {
        if (str_sub(name, 1, 4) == "Perc") {
            formatted_value <- format_as_percentage(value)
            value <- value * 100
        }
        else if(str_sub(name, 1, 5) == "Count") {
            formatted_value <- format_as_absolute(value)
        }
        else if(str_sub(name, 1, 4) == "Size") {
            formatted_value <- format_as_size(value)
        }
        else {
            stop(str_c("Unidentified name", name, sep = " "))
        }

        formatted_value <- str_replace(formatted_value, "\\%", "\\\\%")

        str_glue("\\newcommand {{\\{name}}} {{{formatted_value}}} %% {value}")
    }

    info("=> Latexing to '", settings$output_filepath, "'\n")

    computer <- eval(as.symbol(settings$analysis))

    separator <- str_c(rep("%", 80), collapse = "")

    latex_file <- file(settings$output_filepath,
                       c("wt", "at")[settings$append + 1])

    if (settings$append) {
        info("=> Appending to existing content in '", settings$output_filepath, "'\n")
    }
    else {
        info("=> Overwriting existing content in '", settings$output_filepath, "'\n")

        write_lines(separator, latex_file)

        write_lines(str_c("%% AUTOGENERATED, DO NOT MODIFY BY HAND"),
                    latex_file)

        write_lines(separator, latex_file)

        write_lines(c(""), latex_file)
    }


    write_lines(c(separator,
                  str_c("%%", settings$analysis, sep = " "),
                  separator),
                latex_file)

    computer(analyses) %>%
        imap_chr(to_latex_command) %>%
        write_lines(latex_file)

    write_lines("", latex_file)

    close(latex_file)

    info("=> Latexed to '", settings$output_filepath, "'\n")
}


scan_input_directory <- function(settings) {

    info("=> Scanning ", settings$input_dirpath, "\n")

    summarized_data_file_glob <-
        str_c("*",
              data_table_extension(settings$binary,
                                   settings$compression_level))

    summarized_data_filepaths <-
        settings$input_dirpath %>%
        dir_ls(recursive = FALSE, type = "file", glob = summarized_data_file_glob) %>%
        path_ext_remove() %>%
        path_ext_remove()

    analyses <- new.env(parent = emptyenv(), hash = TRUE)

    map(summarized_data_filepaths,
        function(summarized_data_filepath) {
            delayedAssign(path_file(summarized_data_filepath),
                          read_data_table(summarized_data_filepath,
                                          binary = settings$binary,
                                          compression_level = settings$compression_level),
                          assign.env = analyses)
        })

    info("=> Scanned ", settings$input_dirpath, "\n")

    analyses
}


parse_program_arguments <- function() {

    usage <- "%prog summarized-output-dirpath visualized-output-dirpath"
    description <- paste(
        "summarized-output-dirpath  directory containing summarized data files",
        "latex-output-filepath      file to which latex definitions will be exported",
        "analysis                   analysis whose variables will be exported",
         sep = "\n")

    option_list <- list(
        make_option(c("--append"),
                    action = "store_true",
                    default = FALSE,
                    help = "append to file",
                    metavar = "append"),

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
         output_filepath = arguments$args[2],
         analysis = arguments$args[3],
         append = arguments$options$append,
         binary = arguments$options$binary,
         compression_level = arguments$options$compression_level)
}


main <- function() {
    settings <- parse_program_arguments()

    print(settings)

    analyses <- scan_input_directory(settings)

    latex(analyses, settings)
}


main()
