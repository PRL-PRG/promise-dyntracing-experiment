suppressPackageStartupMessages(library("optparse"))
suppressPackageStartupMessages(library("dplyr"))
suppressPackageStartupMessages(library("stringr"))
suppressPackageStartupMessages(library("compiler"))
suppressPackageStartupMessages(library("fs"))
suppressPackageStartupMessages(library("purrr"))
suppressPackageStartupMessages(library("readr"))
suppressPackageStartupMessages(library("magrittr"))
suppressPackageStartupMessages(library("processx"))


parse_command_line <- function() {

    option_list <- list(

        make_option(c("--tracing-timeout"),
                    action = "store",
                    type = "integer",
                    default = 60 * 60,
                    help="Timeout for tracing a script",
                    metavar="tracing-timeout"),

        make_option(c("--r-dyntrace"),
                    action="store",
                    type="character",
                    help="",
                    metavar="r-dyntrace"),

        make_option(c("--corpus-dirpath"),
                    action="store",
                    type="character",
                    help="",
                    metavar="corpus-dirpath"),

        make_option(c("--raw-analysis-dirpath"),
                    action="store",
                    type="character",
                    help="Output directory for raw tracer analysis (*.tdf)",
                    metavar="raw-analysis-dirpath"),

        make_option(c("--verbose"),
                    action="store_true",
                    default=FALSE,
                    help="Flag to enable verbose mode.",
                    metavar="verbose"),

        make_option(c("--truncate"),
                    action="store_true",
                    default=FALSE,
                    help="Flag to enable overwriting of trace files",
                    metavar="truncate"),

        make_option(c("--binary"),
                    action="store_true",
                    default = FALSE,
                    help="Output data format",
                    metavar="binary"),

        make_option(c("--compression-level"),
                    action="store",
                    type="integer",
                    default=1,
                    help="Compression level for ZSTD streaming compression",
                    metavar="compression-level")
    )

    args <- parse_args(OptionParser(option_list = option_list),
                       positional_arguments=TRUE)

    list(package = args$args[1],
         tracing_timeout = args$options$`tracing-timeout`,
         r_dyntrace = path(getwd(), path_tidy(args$options$`r-dyntrace`)),
         corpus_dirpath = path(getwd(), path_tidy(args$options$`corpus-dirpath`)),
         raw_analysis_dirpath = path(getwd(), path_tidy(args$options$`raw-analysis-dirpath`)),
         verbose = args$options$verbose,
         truncate = args$options$truncate,
         binary = args$options$binary,
         compression_level = args$options$`compression-level`)
}


build_vignettes <- function(settings) {

    vignettes <- tools::pkgVignettes(settings$package, source=TRUE)

    ## if there are vignettes and there are no vignette sources,
    ## then compile the vignettes to sources. This compilation
    ## will result in .R files in the doc directory of package
    ## and will be picked up by the next step of the program
    if (length(vignettes$docs) != 0 &&
        length(vignettes$sources) == 0) {

        tools::checkVignettes(settings$package,
                              find.package(settings$package)[1],
                              tangle = TRUE,
                              weave = FALSE,
                              workdir = "src")
    }
}

copy_vignettes <- function(settings) {

    build_vignettes(settings)

    destination_paths <- path(settings$corpus_dirpath,
                              settings$package,
                              c("doc", "data"))

    vignettes <- vignette(package = settings$package)$results

    if(nrow(vignettes) == 0) {
        dir_create(destination_paths)
    }

    else {
        source_paths <- path(vignettes[1, "LibPath"],
                             vignettes[1, "Package"],
                             c("doc", "data"))
        if(dir_exists(source_paths[1]))
            dir_copy(source_paths[1], destination_paths[1])
        if(dir_exists(source_paths[2]))
            dir_copy(source_paths[2], destination_paths[2])
    }

    "doc"
}

copy_tests <- function(settings) {
    destination_path <- path(settings$corpus_dirpath, settings$package, "tests")

    source_path <- path(find.package(settings$package), "tests")

    if(!dir_exists(source_path)) {
        dir_create(destination_path)
    } else {
        dir_copy(source_path, destination_path)
    }

    "tests"
}


copy_examples <- function(settings) {
    destination_path <- path(settings$corpus_dirpath,
                             settings$package,
                             "examples")

    dir_create(destination_path)

    db <- tryCatch({
        tools::Rd_db(settings$package)
    }, error=function(e) {
        print(e)
        list()
    })

    iwalk(db, function(rd_data, rd_name) {

        example_filepath <-
            destination_path %>%
            path(path_file(rd_name)) %>%
            path_ext_set("R")

        tools::Rd2ex(rd_data, example_filepath, defines=NULL)

        if(file_exists(example_filepath)) {

            new_content <-
                str_c(str_glue("library({settings$package})"),
                      "",
                      read_file(example_filepath),
                      sep = "\n")

            write_file(new_content,
                       example_filepath)
        }
    })

    "examples"
}


indent <- function(lines, spaces = 4) {
    indentation <- paste0(rep(" ", spaces), collapse = "")
    ifelse(lines == "",
           "",
           paste0(indentation, lines))
}


wrap_script <- function(settings, script_dirname, script_filename) {

    script_dirpath <- path(settings$corpus_dirpath,
                           settings$package,
                           script_dirname)

    script_filepath <- path(script_dirpath, script_filename)

    wrapped_script_filepath <- path(script_dirpath,
                                    str_c("__wrapped__",
                                          script_filename))

    script_code <- str_c(indent(readLines(script_filepath), 4),
                         collapse = "\n")


    raw_output_dirpath <- path(settings$raw_analysis_dirpath,
                               settings$package,
                               script_dirname,
                               path_ext_remove(script_filename))


    dir_create(raw_output_dirpath)

    wrapped_code <- str_glue(
        "setwd('{script_dirpath}')",
        "library(promisedyntracer)",
        "",
        "dyntrace_promises({{",
        "{script_code}",
        "}}",
        ", '{raw_output_dirpath}'",
        ", verbose = {settings$verbose}",
        ", truncate = {settings$truncate}",
        ", binary = {settings$binary}",
        ", compression_level = {settings$compression_level})",
        .sep = "\n")

    writeLines(wrapped_code, con = wrapped_script_filepath)

    tibble(filepath = wrapped_script_filepath,
           raw_analysis_dirpath = raw_output_dirpath)
}

wrap_scripts <- function(settings, script_dirname) {


    path(settings$corpus_dirpath, settings$package, script_dirname) %>%
        dir_ls(type = "file", glob = "*.R") %>%
        path_file() %>%
        map_dfr(
            function(script_filename) {
                wrap_script(settings, script_dirname, script_filename)
            }
        )
}

run_script <- function(settings, script_filepath) {

    cat("Executing ", script_filepath, "\n")

    processx::run(command = settings$r_dyntrace,
                  args = str_c("--file=", script_filepath),
                  timeout = settings$tracing_timeout,
                  cleanup_tree = TRUE,
                  echo = TRUE, echo_cmd = TRUE)

    script_filepath
}

run_scripts <- function(settings, script_filepaths) {
    script_filepaths %>%
        map(function(script_filepath) {
            run_script(settings, script_filepath)
        })
}


main <- function() {

    settings <- parse_command_line()

    print(settings)

    ## copy scripts
    vignette_dirname <- copy_vignettes(settings)
    example_dirname <- copy_examples(settings)
    test_dirname <- copy_tests(settings)

    ## wrap scripts
    wrapped_vignettes <- wrap_scripts(settings, vignette_dirname)
    wrapped_examples <- wrap_scripts(settings, example_dirname)
    wrapped_tests <- wrap_scripts(settings, test_dirname)

    ## run scripts
    run_scripts(settings, wrapped_vignettes$filepath)
    run_scripts(settings, wrapped_examples$filepath)
    run_scripts(settings, wrapped_tests$filepath)

}

main()
