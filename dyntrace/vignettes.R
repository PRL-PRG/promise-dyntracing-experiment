suppressPackageStartupMessages(library("optparse"))
suppressPackageStartupMessages(library("dplyr"))
suppressPackageStartupMessages(library("stringr"))
suppressPackageStartupMessages(library("compiler"))

root_dir = paste("traces",
                 "promises",
                 format(Sys.time(), "%Y-%m-%d-%H-%M-%S"),
                 sep="/")

option_list <- list(
  make_option(c("-c", "--command"), action="store", type="character",
              default="~/workspace/R-dyntrace/bin/R CMD BATCH",
              help="Command to execute", metavar="command"),
  make_option(c("-o", "--output-dir"), action="store", type="character", default=root_dir,
              help="Output directory for results (*.sqlite, etc) [default].", metavar="output_dir"),
  make_option(c("--compile"), action="store_true", default=FALSE,
              help="compile vignettes before execution [default]", metavar="compile"),
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
              help="Flag to disable side effect analysis", metavar="side-effect-analysis"),
  make_option(c("-v", "--verbose"), action="store_true", default=FALSE,
              help="Flag to enable verbose mode.", metavar="verbose")
)

cfg <- parse_args(OptionParser(option_list=option_list),
                  positional_arguments=TRUE)

analysis_switch <- list(
  enable_metadata_analysis = !cfg$options$`disable-metadata-analysis`,
  enable_object_count_size_analysis = !cfg$options$`disable-object-count-size-analysis`,
  enable_function_analysis = !cfg$options$`disable-function-analysis`,
  enable_promise_type_analysis = !cfg$options$`disable-promise-type-analysis`,
  enable_promise_slot_mutation_analysis = !cfg$options$`disable-promise-slot-mutation-analysis`,
  enable_promise_evaluation_analysis = !cfg$options$`disable-promise-evaluation-analysis`,
  enable_strictness_analysis = !cfg$options$`disable-strictness-analysis`,
  enable_side_effect_analysis = !cfg$options$`disable-side-effect-analysis`)

list_to_string <- function(analysis_switch) {
  ns <- names(analysis_switch)
  vs <- unlist(analysis_switch)
  spaces <- paste0(",\n", paste0(rep(" ", 32), collapse=""))
  paste0("list(", paste(ns, vs, sep="=", collapse=spaces), ")")
}

analysis_switch <- list_to_string(analysis_switch)

instrumented.code.dir <- paste(cfg$options$`output-dir`, "vignettes", sep="/")
suppressWarnings(dir.create(instrumented.code.dir, recursive = TRUE, showWarnings = FALSE))

log.dir <- paste(cfg$options$`output-dir`, "logs", sep="/")
suppressWarnings(dir.create(log.dir, recursive = TRUE, showWarnings = FALSE))

rdt.cmd.head <- function(wd)
  paste(
    "setwd('", wd, "')\n",
    "library(promisedyntracer)\n",
    "\n",
    "dyntrace_promises({\n",
    sep="")

rdt.cmd.tail<- function(trace_filepath, tracer_output_dir)
  paste("\n}\n, '", trace_filepath, "'\n, '", tracer_output_dir,
        "'\n, verbose=", cfg$options$verbose, "\n, enable_trace=", cfg$options$`enable-trace`,
        "\n, truncate=TRUE\n, analysis_switch=list2env(", analysis_switch, "))\n",
        sep="")

remove_error_blocks <- function(lines) {
  error_on_pattern <- "^ *##.*[eE][rR][rR][oO][rR] *= *T(RUE)?"
  section_pattern <- "^ *##"
  prepend <- FALSE
  output <- lines
  i <- 0
  for (line in lines) {
    i <- i + 1
    error_on_pattern_found <- str_detect(line, error_on_pattern)

    if (!prepend && error_on_pattern_found) 
      prepend <- TRUE

    if (prepend && !error_on_pattern_found && str_detect(line, section_pattern)) 
      prepend <- FALSE

    if (prepend)
      output[i] <- paste("#-#  ", line)
  }

  output
}

instrument_error_blocks_with_try <- function(lines) { 
  error_on_pattern <- "^[ \t]*##.*[eE][rR][rR][oO][rR] *= *T(RUE)?"
  section_pattern <- "^[ \t]*##"
  prepend <- FALSE
  output <- lines
  i <- 0
  for (line in lines) {
    i <- i + 1
    error_on_pattern_found <- str_detect(line, error_on_pattern)
    
    if (!prepend && error_on_pattern_found) {
      prepend <- TRUE
      output[i] <- paste0("try({", line)
    } else if (prepend && error_on_pattern_found) {
      output[i] <- paste0("}); try({", line)
    } else if (prepend && !error_on_pattern_found && str_detect(line, section_pattern)) {
      prepend <- FALSE
      output[i] <- paste0("})", line)
    }
  }
  
  if (prepend) 
    output[length(output)] <- paste0(output[length(output)], "}, silent=TRUE)")
  
  output
}

instrument.vignettes <- function(packages) {

  new_packages <- setdiff(packages, rownames(installed.packages()))
  if (length(new_packages) > 0) {
    install.packages(new_packages,
                     repos='http://cran.us.r-project.org',
                     #Ncpus=20,                            # run in parallel using 20 cores
                     #keep_outputs=T,                      # keeps outputs in ".out" files in current directory
                     INSTALL_opts=c(
                     "--byte-compile",                     # byte-compile packages
                     "--example",                          # extract and keep examples
                     "--install-tests",                    # copy and retain test directory for the package
                     "--with-keep.source",                 # keep line numbers
                     "--no-multiarch"),
                     dependencies = c("Depends",
                                      "Imports",
                                      "LinkingTo",
                                      "Suggests",
                                      "Enhances"))
  }

  i.packages <- 0
  n.packages <- length(packages)
  total.vignettes <- 0

  instrumented.vignette.paths <- list()

  for (package in packages) {
    i.packages <- i.packages + 1

    write(paste("Instrumenting vignettes for package: ", package, sep=""), stdout())

    result.set <- vignette(package = package)
    vignettes.in.package <- result.set$results[,3]
    
    i.vignettes = 0
    n.vignettes = length(vignettes.in.package)
    
    vignette.dirs <- unique(paste(result.set$results[,2], result.set$results[,1], sep="/"))
    instrumented.code.dir.for.package <- paste0(instrumented.code.dir, "/", package)
    
    print(vignette.dirs)
    for (vignette.dir in vignette.dirs) {
      write(paste0("Copying files from ", vignette.dir, " to ", instrumented.code.dir.for.package), stdout())
      dir.create(instrumented.code.dir.for.package, recursive=TRUE)
      for (file in list.files(paste0(vignette.dir, "/doc"), all.files=TRUE, include.dirs=TRUE, no..=TRUE)) {
        destination <- paste0(instrumented.code.dir.for.package, "/", file)
        source <- paste0(vignette.dir, "/doc/", file)
        write(paste0("  * ", source, " -> ", destination), stdout())
        file.copy(from=source, to=destination, recursive=TRUE, overwrite=TRUE)
      }
      for (file in list.files(paste0(vignette.dir, "/data"), all.files=TRUE, include.dirs=TRUE, no..=TRUE)) {
        destination <- paste0(instrumented.code.dir.for.package, "/", file)
        source <- paste0(vignette.dir, "/data/", file)
        write(paste0("  * ", source, " -> ", destination), stdout())
        file.copy(from=source, to=destination, recursive=TRUE, overwrite=TRUE)
      }
    }

    for (vignette.name in vignettes.in.package) {
      trace_filepath <- paste(cfg$options$`output-dir`, "/data/", package, "-", vignette.name, ".trace", sep="")
      tracer.ok.path <- paste(cfg$options$`output-dir`, "/data/", package, "-", vignette.name, ".OK", sep="")
      i.vignettes <- i.vignettes + 1
      total.vignettes <- total.vignettes + 1

      tracer_output_dir <-paste0(cfg$options$`output-dir`, "/output/analysis/", package, "/", vignette.name, sep="", collapse="")
      dir.create(tracer_output_dir, recursive = TRUE, showWarnings = FALSE)

      write(paste("[", i.vignettes, "/", n.vignettes, "] Instrumenting vignette: ", vignette.name, " from ", package, sep=""), stdout())

      one.vignette <- vignette(vignette.name, package = package)
      vignette.code.path <- paste(one.vignette$Dir, "doc", one.vignette$R, sep="/")
      instrumented.code.path <- paste(instrumented.code.dir, "/", package, "/_instrumented_", vignette.name, ".R", sep="")

      write(paste("[", i.vignettes, "/", n.vignettes, "] Writing vignette to: ", instrumented.code.path, sep=""), stdout())

      dir.create(file.path(tracer_output_dir, "functions"))
      vignette.code <- readLines(vignette.code.path)
      instrumented.code <- c(rdt.cmd.head(paste0(instrumented.code.dir, "/", package)),
                             paste0("    ", instrument_error_blocks_with_try(vignette.code)),
                             rdt.cmd.tail(trace_filepath, tracer_output_dir))

      write(instrumented.code, instrumented.code.path)

      write(paste("[", i.vignettes, "/", n.vignettes, "] Done instrumenting vignette: ", vignette.name, " from ", package, sep=""), stdout())

      if (cfg$options$compile) {
        instrumented.code.path.compiled <- paste(tools::file_path_sans_ext(instrumented.code.path), "Rc", sep=".")
        cmpfile(instrumented.code.path, instrumented.code.path.compiled)
        
        instrumented.code.path.loader <- paste(tools::file_path_sans_ext(instrumented.code.path), "load", "R", sep=".")
        write(paste("loadcmp('", tools::file_path_as_absolute(instrumented.code.path.compiled), "')", sep=""), file=instrumented.code.path.loader)
        
        instrumented.vignette.paths[[ total.vignettes ]] <- c(package, vignette.name, instrumented.code.path.loader)
      } else {
        instrumented.vignette.paths[[ total.vignettes ]] <- c(package, vignette.name, instrumented.code.path)
      }
    }
    
    write(paste("Done vignettes for package: ", package, sep=""), stdout())
  }
  
  instrumented.vignette.paths
}

execute.external.programs <- function(program.list, new.process=FALSE) {
  i.programs <- 0
  n.programs <- length(program.list)
  
  for(program in program.list) {
    i.programs <- i.programs + 1
    
    package.name <- program[1]
    vignette.name <- program[2]
    program.path <- program[3]
    
    write(paste("[", i.programs, "/", n.programs, "] Executing file: ", program.path, sep=""), stdout())
    
    log.out.path <- paste(log.dir, "/", i.programs, "_", package.name, "_", vignette.name, ".out", sep="")
    log.err.path <- paste(log.dir, "/", i.programs, "_", package.name, "_", vignette.name, ".err", sep="")
    
    if(new.process) {
      cmd.with.args <- paste(cfg$options$command, program.path, log.out.path, log.err.path, sep=" ")
      system2(cmd.with.args, env=sys.env, wait=TRUE)
      write(cmd.with.args, stdout())
    } else {
      source(program.path, local=new.env()) #local=attach(NULL))
    }
    
    write(paste("[", i.programs, "/", n.programs, "] Done executing file: ", program.path, sep=""), stdout())
  }
}

if (length(cfg$args) > 0)
  execute.external.programs(instrument.vignettes(packages=cfg$args), new.process = FALSE)
