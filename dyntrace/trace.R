
suppressPackageStartupMessages(library("optparse"))
suppressPackageStartupMessages(library("dplyr"))
suppressPackageStartupMessages(library("stringr"))
suppressPackageStartupMessages(library("compiler"))
suppressPackageStartupMessages(library("fs"))

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

copy_vignettes <- function(settings) {

    destination_paths <- path(settings$corpus_dir,
                              settings$package,
                              c("doc", "data"))
    print(destination_paths)

    vignettes <- vignette(package = settings$package)$results

    if(nrow(vignettes) == 0) {
        dir_create(destination_paths)
    }
    else {
        source_paths <- path(vignettes[1, "LibPath"],
                             vignettes[1, "Package"],
                             c("doc", "data"))
        print(source_paths)
        if(dir_exists(source_paths[1]))
            dir_copy(source_paths[1], destination_paths[1])
        if(dir_exists(source_paths[2]))
            dir_copy(source_paths[2], destination_paths[2])
    }

  destination_paths[1]
  ## WARN - Ideally, this should only be a list of size 1
  ##        A list of size greater than 1 implies the package
  ##        has multiple installation locations or has its
  ##        vignettes split up across multiple directories.
  ##        This seems unreasonable.

  ## source_doc_paths <- path(package_paths, "doc")
  ## source_data_paths <- path(package_paths, "data")

  ## destination_package_path <- path(settings$vignette_dir, package)
  ## destination_doc_path <- path(destination_package_path, "doc")
  ## destination_data_path <- path(destination_package_path, "data")

  ## dir_copy(source_data_paths, destination_data_path)
  ## dir_copy(source_doc_paths, destination_doc_path)
}

wrap_code <- function(settings, code) {
  paste(
    "setwd('", wd, "')\n",
    "library(promisedyntracer)\n",
    "\n",
    "dyntrace_promises({\n",
    code,
    "\n}\n, '", trace_filepath, "'\n, '", tracer_output_dir,
    "'\n, verbose=", settings$verbose, "\n, enable_trace=", settings$trace,
    "\n, truncate=TRUE\n, analysis_switch=list2env(", settings$analysis_switch_string, "))\n",
    sep=""
  )
}

wrap_vignette <- function(settings, corpus_dirpath) {

    indent <- function(lines, spaces = 4) {
        indentation <- paste0(rep(" ", spaces), collapse = "")
        ifelse(lines == "",
               "",
               paste0(indentation, lines))
    }

    list_to_string <- function(analysis_switch) {
        ns <- names(analysis_switch)
        vs <- unlist(analysis_switch)
        spaces <- paste0(",\n", paste0(rep(" ", 34), collapse=""))
        paste0("list(", paste(ns, vs, sep="=", collapse=spaces), ")")
    }


    vignette_items <- vignette(package=settings$package)$results[,"Item"]

    if(length(vignette_items) == 0)
        return(c())

    vignette_filenames <- str_c(vignette_items, ".R")


    wrapper <- Vectorize(function(vignette_filename) {

        vignette_filepath <- path(corpus_dirpath, vignette_filename)

        wrapped_vignette_filepath <- path(corpus_dirpath,
                                          str_c("__wrapped__",
                                                vignette_filename))

        print(vignette_filepath)
        vignette_code <- str_c(indent(readLines(vignette_filepath), 4),
                               collapse = "\n")

        print(vignette_code)

        raw_analysis_dirpath <- path(settings$raw_analysis_dir,
                                     settings$package,
                                     path_ext_remove(vignette_filename))

        dir_create(raw_analysis_dirpath)

        function_dirpath <- path(raw_analysis_dirpath, "functions")

        dir_create(function_dirpath)

        analysis_switch <- list_to_string(settings$analysis_switch)

        trace_dirpath <- path(settings$trace_dir, "lazy", settings$package)

        dir_create(trace_dirpath, recursive = TRUE)

        trace_filepath <- path(trace_dirpath,
                               str_c(path_ext_remove(vignette_filename), ".trace"))

        print(trace_filepath)
        wrapped_code <- str_glue(
            "setwd('{corpus_dirpath}')",
            "library(promisedyntracer)",
            "",
            "dyntrace_promises({{",
            "{vignette_code}",
            "}}",
            ", '{trace_filepath}'",
            ", '{raw_analysis_dirpath}'",
            ", verbose = {settings$verbose}",
            ", enable_trace = {settings$trace}",
            ", truncate = {settings$truncate}",
            ", analysis_switch = list2env({analysis_switch}))",
            .sep = "\n")

        print(wrapped_code)

        writeLines(wrapped_code, con = wrapped_vignette_filepath)

        wrapped_vignette_filepath

    }, vectorize.args=c("vignette_filename"))

    wrapper(vignette_filenames)
}


run <- function(settings, wrapped_vignette_filepaths) {
    execute <- Vectorize(function(filepath) {
        system2(command = settings$r_dyntrace,
                args = str_glue("--file={filepath}"))
    }, vectorize.args = c("filepath"))

    execute(wrapped_vignette_filepaths)
    wrapped_vignette_filepaths
}

main <- function() {
    print("In main")
    settings <- parse_command_line()
    print(settings)
                                        #run_vignette(settings,
    corpus_dirpath <- copy_vignettes(settings)
    wrapped_vignette_filepaths <- wrap_vignette(settings, corpus_dirpath)
    run(settings, wrapped_vignette_filepaths)
}

main()
