suppressPackageStartupMessages(library("optparse"))
suppressPackageStartupMessages(library("dplyr"))
suppressPackageStartupMessages(library("stringr"))
suppressPackageStartupMessages(library("compiler"))
suppressPackageStartupMessages(library("fs"))
suppressPackageStartupMessages(library("purrr"))
suppressPackageStartupMessages(library("readr"))
suppressPackageStartupMessages(library("magrittr"))


## Timeout is a value in seconds, used by the system2 command.
## Currently, it is set to 1 hour
TRACING_RUNTIME_TIMEOUT <- 60 * 60

parse_command_line <- function() {

    option_list <- list(
        make_option(c("-c", "--command"), action="store", type="character",
                    default="echo", help="Command to execute", metavar="command"),
        make_option(c("--compile"), action="store_true", default=FALSE,
                    help="compile vignettes before execution [default]", metavar="compile"),
        make_option(c("-v", "--verbose"), action="store_true", default=FALSE,
                    help="Flag to enable verbose mode.", metavar="verbose"),
        make_option(c("-t", "--truncate"), action="store_true", default=FALSE,
                    help="Flag to enable overwriting of trace files", metavar="truncate"),

        make_option(c("--r-dyntrace"), action="store", type="character",
                    help="", metavar="r-dyntrace"),

        make_option(c("--corpus-dir"), action="store", type="character",
                    help="", metavar="corpus-dir"),

        make_option(c("--trace-dir"), action="store", type="character",
                    help="Output directory for raw traces", metavar="trace-dir"),

        make_option(c("--raw-analysis-dir"), action="store", type="character",
                    help="Output directory for raw tracer analysis (*.tdf)", metavar="raw-analysis-dir"),

        make_option(c("--enable-trace"), action="store_true", default=FALSE,
                    help="Flag to enable trace files (*.trace) [default].", metavar="enable-trace"),

        make_option(c("--binary"), action="store_true", default = FALSE,
                    help="Output data format", metavar="binary"),

        make_option(c("--compression-level"), action="store", type="integer", default=1,
                    help="Compression level for ZSTD streaming compression", metavar="compression-level"),

        make_option(c("--disable-metadata-analysis"), action="store_true", default=FALSE,
                    help="Flag to disable metadata analysis", metavar="metadata-analysis"),
        make_option(c("--disable-object-count-size-analysis"), action="store_true", default=FALSE,
                    help="Flag to disable object count size analysis", metavar="object-count-size-analysis"),
        make_option(c("--disable-function-analysis"), action="store_true", default=FALSE,
                    help="Flag to disable function analysis", metavar="function-analysis"),
        make_option(c("--disable-promise-type-analysis"), action="store_true", default=FALSE,
                    help="Flag to disable promise type analysis", metavar="promise-type-analysis"),
        make_option(c("--disable-promise-slot-mutation-analysis"), action="store_true", default=FALSE,
                    help="Flag to disable promise slot mutation analysis", metavar="promise-slot-mutation-analysis"),
        make_option(c("--disable-promise-evaluation-analysis"), action="store_true", default=FALSE,
                    help="Flag to disable promise evaluation analysis", metavar="promise-evaluation-analysis"),
        make_option(c("--disable-strictness-analysis"), action="store_true", default=FALSE,
                    help="Flag to disable strictness analysis", metavar="strictness-analysis"),
        make_option(c("--disable-side-effect-analysis"), action="store_true", default=FALSE,
                    help="Flag to disable side effect analysis", metavar="side-effect-analysis")
    )

    args <- parse_args(OptionParser(option_list = option_list),
                       positional_arguments=TRUE)

    list(package = args$args[1],
         compile = args$options$compile,
         verbose = args$options$verbose,
         truncate = args$options$truncate,
         trace = args$options$`enable-trace`,
         r_dyntrace = path(getwd(), path_tidy(args$options$`r-dyntrace`)),
         corpus_dir = path(getwd(), path_tidy(args$options$`corpus-dir`)),
         trace_dir = path(getwd(), path_tidy(args$options$`trace-dir`)),
         raw_analysis_dir = path(getwd(), path_tidy(args$options$`raw-analysis-dir`)),
         binary = args$options$binary,
         compression_level = args$options$`compression-level`,
         analysis_switch = list(
             enable_metadata_analysis = !args$options$`disable-metadata-analysis`,
             enable_object_count_size_analysis = !args$options$`disable-object-count-size-analysis`,
             enable_function_analysis = !args$options$`disable-function-analysis`,
             enable_promise_type_analysis = !args$options$`disable-promise-type-analysis`,
             enable_promise_slot_mutation_analysis = !args$options$`disable-promise-slot-mutation-analysis`,
             enable_promise_evaluation_analysis = !args$options$`disable-promise-evaluation-analysis`,
             enable_strictness_analysis = !args$options$`disable-strictness-analysis`,
             enable_side_effect_analysis = !args$options$`disable-side-effect-analysis`))
}


build_vignettes <- function(settings) {

    vignettes <- tools::pkgVignettes(settings$package, source=T)

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

    destination_paths <- path(settings$corpus_dir,
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
    destination_path <- path(settings$corpus_dir, settings$package, "tests")

    source_path <- path(find.package(settings$package), "tests")

    if(!dir_exists(source_path)) {
        dir_create(destination_path)
    } else {
        dir_copy(source_path, destination_path)
    }

    "tests"
}


copy_examples <- function(settings) {
    destination_path <- path(settings$corpus_dir,
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


list_to_string <- function(lst) {
    ns <- names(lst)
    vs <- unlist(lst)
    spaces <- paste0(",\n", paste0(rep(" ", 34), collapse=""))
    paste0("list(", paste(ns, vs, sep="=", collapse=spaces), ")")
}


wrap_script <- function(settings, script_dirname, script_filename) {

    script_dirpath <- path(settings$corpus_dir,
                           settings$package,
                           script_dirname)

    script_filepath <- path(script_dirpath, script_filename)

    wrapped_script_filepath <- path(script_dirpath,
                                    str_c("__wrapped__",
                                          script_filename))

    script_code <- str_c(indent(readLines(script_filepath), 4),
                         collapse = "\n")


    raw_output_dirpath <- path(settings$raw_analysis_dir,
                               settings$package,
                               script_dirname,
                               path_ext_remove(script_filename))


    dir_create(raw_output_dirpath)

    function_dirpath <- path(raw_output_dirpath, "functions")

    dir_create(function_dirpath)

    analysis_switch <- list_to_string(settings$analysis_switch)

    trace_dirpath <- path(settings$trace_dir, "lazy-traces", settings$package)

    dir_create(trace_dirpath, recursive = TRUE)

    trace_filepath <- path(trace_dirpath,
                           str_c(path_ext_remove(script_filename), ".trace"))

    wrapped_code <- str_glue(
        "setwd('{script_dirpath}')",
        "library(promisedyntracer)",
        "",
        "dyntrace_promises({{",
        "{script_code}",
        "}}",
        ", '{trace_filepath}'",
        ", '{raw_output_dirpath}'",
        ", verbose = {settings$verbose}",
        ", enable_trace = {settings$trace}",
        ", truncate = {settings$truncate}",
        ", binary = {settings$binary}",
        ", compression_level = {settings$compression_level}",
        ", analysis_switch = list2env({analysis_switch}))",
        .sep = "\n")

    writeLines(wrapped_code, con = wrapped_script_filepath)

    tibble(filepath = wrapped_script_filepath,
           raw_analysis_dirpath = raw_output_dirpath)
}

wrap_scripts <- function(settings, script_dirname) {


    path(settings$corpus_dir, settings$package, script_dirname) %>%
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

    system2(command = settings$r_dyntrace,
            args = str_c("--file=", script_filepath),
            timeout = TRACING_RUNTIME_TIMEOUT)

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
