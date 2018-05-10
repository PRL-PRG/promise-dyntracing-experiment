suppressPackageStartupMessages(library("optparse"))
suppressPackageStartupMessages(library("compiler"))
suppressPackageStartupMessages(library("tidyr"))
suppressPackageStartupMessages(library("stringr"))
suppressPackageStartupMessages(library("tibble"))
suppressPackageStartupMessages(library("dplyr"))

log <- function(..., sinks=c(stdout())) 
  invisible(sapply(sinks, 
            function(sink) write(paste(c(...), sep="", collapse=""), sink)))

package_information_definition <- 
  tibble(type=character(), package=character(), item=character(), 
         directory=character(), path=character(), code=list(), lines=numeric())

get_all_package_information <- function(packages, vignettes, examples, tests) {
  log("Retrieving information about runnables in packages")
  get_package_information_ <- function(package) 
    get_package_information(package, 
                            vignettes=vignettes, 
                            examples=examples, 
                            tests=tests)
  bind_rows(lapply(packages, get_package_information_))
}

get_package_information <- function(package, vignettes=T, examples=T, tests=T)
  rbind(if (vignettes) get_vignette_information(package) 
        else package_information_definition,
        if (examples) get_example_information(package) 
        else package_information_definition,
        if (tests) get_test_information(package) 
        else package_information_definition)

get_test_information <- function(package) {
  #log("Retrieve test case information for ", package)
  
  package_dir <- find.package(package)
  package_dir <- paste0("/home/kondziu/R_LIBS/", package) # FIXME remove
  
  test_dir <- file.path(package_dir, "tests")
  test_paths <- list.files(test_dir, ".R", full.names=TRUE)
  test_items <- tools::file_path_sans_ext(basename(test_paths))
  code_lines <- lapply(test_paths, readLines)
  lines <- sapply(code_lines, length)
  codes <- sapply(code_lines, function(code) paste(code, collapse="\n"))
  
  log("  * found ", length(test_items), " test scripts for ", package)
  
  tibble(type="test", package=package, item=test_items, directory=test_dir, 
         path=test_paths, code=codes, lines=lines)
}

get_vignette_information <- function(package) {
  #log("Retrieve vignette information for ", package)
  
  vignettes <- vignette(package = package)$results
  vignette_packages <- vignettes[,1]
  vignette_items <- vignettes[,3]
  vignette_dirs <- file.path(vignettes[,2], vignettes[,1], "doc")
  vignette_paths <- file.path(vignette_dirs, paste0(vignette_items, ".R"))
  code_lines <- lapply(lapply(vignette_paths, readLines), instrument_error_blocks)
  lines <- sapply(code_lines, length)
  codes <- sapply(code_lines, function(code) paste(code, collapse="\n"))
  
  log("  * found ", length(vignette_items), " vignettes for ", package)
  
  tibble(type="vignette", package=vignette_packages, item=vignette_items, 
         directory=vignette_dirs, path=vignette_paths, code=codes, lines=lines)
}

get_example_information <- function(package) {
  #log("Retrieve example information for ", package)
  
  package_dir <- find.package(package)
  package_dir <- paste0("/home/kondziu/R_LIBS/", package) # FIXME remove
  
  examples <- tryCatch(tools::Rd_db(basename(package_dir), 
                                    lib.loc=dirname(package_dir)), 
                       error=function(e) c())
  
  example_files <- names(examples)
  example_items <- tools::file_path_sans_ext(example_files)
  example_dir <- file.path(package_dir, "examples")
  example_paths <- file.path(example_dir, example_files)
  
  code_lines <- lapply(example_paths, function(file) {
    name <- tools::file_path_sans_ext(basename(file))
    example_path <- tempfile(paste0(name,"-XXXX-Ex.R"))
    tools::Rd2ex(examples[[basename(file)]], example_path, defines=NULL)
    if (!file.exists(example_path)) character()
    else c(paste0("library(", package, ")"), 
           readLines(example_path))
  })
  lines <- sapply(code_lines, length)
  codes <- sapply(code_lines, function(code) paste(code, collapse="\n"))
  
  log("  * found ", length(example_items), " examples for ", package)
  
  tibble(type="example", package=package, item=example_items, 
         directory=example_dir, path=example_paths, code=codes, lines=lines)
}

instrument_error_blocks <- function(lines) { 
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
    } else if (prepend && !error_on_pattern_found && 
               str_detect(line, section_pattern)) {
      prepend <- FALSE
      output[i] <- paste0("})", line)
    }
  }
  
  if (prepend) 
    output[length(output)] <- paste0(output[length(output)], "}, silent=TRUE)")
  
  output
}

instrument_runnables <- function(runnables, output_dir) {
  log("Instrument runnables")
  type_dir <-  paste0(runnables$type, "s")
  working_dirs <- file.path(output_dir, "instrumented", type_dir, 
                            runnables$package)
  database_filenames <- paste0(runnables$package, "::", runnables$item, "::", 
                               runnables$type, ".sqlite")
  database_paths <- file.path(output_dir, "data", database_filenames)
  instrumented_code <- instrument_code(runnables$code, working_dirs, 
                                       database_paths)
  
  runnables$working_dir <- working_dirs
  runnables$database_path <- database_paths
  runnables$instrumented_code <- instrumented_code
  runnables$number <- c(1:length(runnables$type))
  
  runnables
}

instrument_code <- function(code, working_directory, database_path) {
  header <- paste0("setwd('", working_directory, "')\n", 
                   "library(promisedyntracer)\n",  
                   "dyntrace_promises({\n")
  footer <- paste0("\n}, ",
                   "database='", database_path, "', ", 
                   "verbose=0, " ,
                   "truncate=TRUE)")
  paste(header, code, footer, sep="")
}

# Helper function that traverses two vectors and applies function to each pair
pairwise <- function(a, b, f) {
  pairs <- mapply(c, a, b, SIMPLIFY=F)
  sapply(pairs, function(pair) f(pair[1], pair[2]))
}

output_code <- function(runnables, compile) {
  # Create working directories
  log("Create working directories")
  for (dir in unique(runnables$working_dir)) {
    log("  * ", dir)
    dir.create(dir, recursive=T, showWarnings=F)
  }
  
  # Copy files from input directories to working directories
  log("Copy files to working directories")
  pairs <- mapply(c, runnables$directory, runnables$working_dir, SIMPLIFY=F)
  unique_pairs <- unique(pairs)
  for (pair in unique_pairs) {
    source_dir <- pair[1]
    destination_dir <- pair[2]
    
    log("Copying from ", source_dir, " to ", destination_dir)
    source_files <- list.files(source_dir, all.files=T, include.dirs=T, no..=T, 
                               full.names=T)
    for (file in source_files) {
      log("  * ", basename(file))
      file.copy(from=file, to=destination_dir, recursive=T, overwrite=T)
    }
  }
  
  # Create DB output directories 
  log("Create output directories")
  for (dir in unique(sapply(runnables$database_path, dirname))) {
    log("  * ", dir)
    dir.create(dir, recursive=T, showWarnings=F)
  }
  
  # Write out code to files
  instrumented_source_paths <- 
    file.path(runnables$working_dir, paste0(basename(runnables$path)))
  
  log("Writing instrumented source code")
  pairwise(instrumented_source_paths, runnables$instrumented_code, 
    function(path, code) {
      log("  * ", path)
      write(code, path)  
    })
  
  # If no compilation required, just point to the source file as the runnable
  if (!compile) 
    runnables$runnable <- instrumented_source_paths
  
  # If compilation is required, compile the source, create a loader, and point
  # to the loader as runnable.
  else {
    # Compile the source to byte-code
    instrumented_compiled_paths <- 
      paste0(tools::file_path_sans_ext(instrumented_source_paths), ".Rc")
    
    log("Compiling source code to byte-code") 
    pairwise(instrumented_source_paths, instrumented_compiled_paths, 
      function(source, destination) {
        log("  * compiling ", basename(source), " as ", basename(destination))
        cat("    "); cmpfile(source, destination, verbose=F)
      })
    
    # Create loaders
    instrumented_loader_paths <- 
      paste0(tools::file_path_sans_ext(instrumented_source_paths), "-Ld.R")
    log("Writing byte-code loaders") 
    pairwise(instrumented_compiled_paths, instrumented_loader_paths, 
      function(compiled_path, loader_path) {
        loader_code <- 
          paste0("loadcmp('", tools::file_path_as_absolute(compiled_path), "')")

        log("  * ", loader_path)
        write(loader_code, loader_path)
      })
    
    # Point to loader as runnable
    runnables$runnable <- instrumented_loader_paths
  }
  
  runnables
}

execute_code <- function(runnables) {
  log("Executing runnables (", length(runnables$number), ")")
  messages <- 
    paste0("  * [", runnables$number, "/", length(runnables$number), 
           "] executing ", runnables$item, " (", runnables$type, 
           " from package ", runnables$package, ")")
  pairwise(messages, runnables$runnable,
    function(message, runnable) {
      log(message)
      source(runnable, local=new.env())
    })
}

# Commandline option parser
option_parser <- OptionParser(option_list=
    list(
      make_option(c("-c", "--command"), action="store", type="character", 
                  default="~/workspace/R-dyntrace/bin/R CMD BATCH",
                  help="R interpreter command", metavar="COMMAND"),
      make_option(c("-o", "--output-dir"), action="store", type="character", 
                  default=getwd(), metavar="OUTPUT_DIR",
                  help="output directory (*.sqlite, etc) [default]."),
      make_option(c("--compile"), action="store_true", default=FALSE,
                  help="compile instrumented code before execution [default]", 
                  metavar="COMPILE"),
      make_option(c("--tests"), action="store_true", default=FALSE,
                  help="run on tests", metavar="TESTS"),      
      make_option(c("--vignettes"), action="store_true", default=FALSE,
                  help="run on vignettes", metavar="VIGNETTES"),
      make_option(c("--examples"), action="store_true", default=FALSE,
                  help="run on examples", metavar="EXAMPLES")))

# Parse commandline options
cfg <- parse_args(option_parser, positional_arguments=TRUE)

# Extract the runnables
runnables <- get_all_package_information(cfg$args, 
                                         vignettes=cfg$options$vignettes, 
                                         examples=cfg$options$examples, 
                                         tests=cfg$options$tests)

# No runnables found, exit.
if (length(runnables$type) == 0) {
  log("No runnables found")
  quit()
}

# Instrument the runnables with the tracer
runnables <- instrument_runnables(runnables, cfg$options$`output-dir`)

# Write instrumented code to disk
runnables <- output_code(runnables, cfg$options$`compile`)

# Execute vignettes (as separate processes, sequentially)
execute_code(runnables)