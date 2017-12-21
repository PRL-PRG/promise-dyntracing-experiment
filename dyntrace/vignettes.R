suppressPackageStartupMessages(library("optparse"))
suppressPackageStartupMessages(library("dplyr"))
suppressPackageStartupMessages(library("stringr"))
suppressPackageStartupMessages(library("compiler"))

#packages = commandArgs(trailingOnly = TRUE)

#cmd <- "~/workspace/R-dyntrace/bin/R CMD BATCH" #paste(shQuote(file.path(R.home("bin"), "R")))
#sys.env <- as.character(c("R_KEEP_PKG_SOURCE=yes", "R_ENABLE_JIT=0"))

root_dir = paste("traces",
                 "promises",
                 format(Sys.time(), "%Y-%m-%d-%H-%M-%S"),
                 sep="/")

option_list <- list(
  make_option(c("-c", "--command"), action="store", type="character", default="~/workspace/R-dyntrace/bin/R CMD BATCH",
              help="Command to execute", metavar="command"),
  make_option(c("-o", "--output-dir"), action="store", type="character", default=root_dir,
              help="Output directory for results (*.sqlite, etc) [default].", metavar="output_dir"),
  make_option(c("--compile"), action="store_true", default=FALSE,
              help="compile vignettes before execution [default]", metavar="compile")
)

cfg <- parse_args(OptionParser(option_list=option_list), positional_arguments=TRUE)

instrumented.code.dir <- paste(cfg$options$`output-dir`, "vignettes", sep="/")
dir.create(instrumented.code.dir, recursive = TRUE, showWarnings = FALSE)

log.dir <- paste(cfg$options$`output-dir`, "logs", sep="/")
dir.create(log.dir, recursive = TRUE, showWarnings = FALSE)

rdt.cmd.head <- function(database_filepath, verbose=TRUE)
  paste(
    "library(promisedyntracer)\n",
    "\n",
    "dyntracer <- create_dyntracer('", database_filepath, "',", "FALSE,", verbose, ")\n",
    "dyntrace(dyntracer, {\n",
    sep="")

rdt.cmd.tail<- function(path)
  paste("\n})\n",
        "destroy_dyntracer(dyntracer)\n",
        "write('OK', '", path, "')\n",
        sep="")

instrument.vignettes <- function(packages) {

  new_packages <- setdiff(packages, rownames(installed.packages()))
  if (length(new_packages) > 0) {
    install.packages(new_packages,
                     repos='http://cran.us.r-project.org',
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
    
    write(paste("[", i.packages, "/", n.packages, "] Instrumenting vignettes for package: ", package, sep=""), stdout())
    
    result.set <- vignette(package = package)
    vignettes.in.package <- result.set$results[,3]
    
    i.vignettes = 0
    n.vignettes = length(vignettes.in.package)
    
    for (vignette.name in vignettes.in.package) {
      tracer.output.path <- paste(cfg$options$`output-dir`, "/data/", package, "-", vignette.name, ".sqlite", sep="")
      tracer.ok.path <- paste(cfg$options$`output-dir`, "/data/", package, "-", vignette.name, ".OK", sep="")
      i.vignettes <- i.vignettes + 1
      total.vignettes <- total.vignettes + 1
      
      write(paste("[", i.packages, "/", n.packages, "::", i.vignettes, "/", n.vignettes, "/", total.vignettes, "] Instrumenting vignette: ", vignette.name, " from ", package, sep=""), stdout())
      
      one.vignette <- vignette(vignette.name, package = package)
      vignette.code.path <- paste(one.vignette$Dir, "doc", one.vignette$R, sep="/")
      dir.create(instrumented.code.dir)
      instrumented.code.path <- paste(instrumented.code.dir, "/", package, "-", vignette.name, ".R", sep="")
      
      write(paste("[", i.packages, "/", n.packages, "::", i.vignettes, "/", n.vignettes, "/", total.vignettes, "] Writing vignette to: ", instrumented.code.path, sep=""), stdout())

      vignette.code <- readLines(vignette.code.path)
      instrumented.code <- c(rdt.cmd.head(tracer.output.path, verbose = FALSE),
                             paste0("    ", vignette.code),
                             rdt.cmd.tail(tracer.ok.path))
                                                
      write(instrumented.code, instrumented.code.path)
      
      write(paste("[", i.packages, "/", n.packages, "::", i.vignettes, "/", n.vignettes, "/", total.vignettes, "] Done instrumenting vignette: ", vignette.name, " from ", package, sep=""), stdout())
      
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
    
    write(paste("[", i.packages, "/", n.packages, "] Done vignettes for package: ", package, sep=""), stdout())
  }
  
  instrumented.vignette.paths
}

# instrument.and.aggregate.vignettes <- function(packages) {
#   i.packages <- 0
#   n.packages <- length(packages)
#   
#   instrumented.vignette.paths <- list()
#   
#   for (package in packages) {
#     i.packages <- i.packages + 1
#     
#     write(paste("[", i.packages, "/", n.packages, "] Instrumenting vignettes for package: ", package, sep=""), stdout())
#     
#     result.set <- vignette(package = package)
#     vignettes.in.package <- result.set$results[,3]
#     
#     instrumented.code.path <- paste(instrumented.code.dir, "/", i.packages, "_", package, ".R", sep="")
#     tracer.output.path <- paste(cfg$options$`output-dir`, "/", package, ".sqlite", sep="")
#     
#     i.vignettes = 0
#     n.vignettes = length(vignettes.in.package)
#     
#     write(rdt.cmd.head(i.vignettes == 1, tracer.output.path), instrumented.code.path, append=FALSE)
# 
#     for (vignette.name in vignettes.in.package) {
#       i.vignettes <- i.vignettes + 1
#       
#       write(paste("[", i.packages, "/", n.packages, "::", i.vignettes, "/", n.vignettes, "] Appending vignette: ", vignette.name, " from ", package, sep=""), stdout())
#       
#       one.vignette <- vignette(vignette.name, package = package)
#       vignette.code.path <- paste(one.vignette$Dir, "doc", one.vignette$R, sep="/")
#       
#       write(paste("[", i.packages, "/", n.packages, "::", i.vignettes, "/", n.vignettes, "] Appending vignette to: ", instrumented.code.path, sep=""), stdout())
#       
#       vignette.code <- readLines(vignette.code.path)
#       write(vignette.code, instrumented.code.path, append=TRUE)
#       
#       write(paste("[", i.packages, "/", n.packages, "::", i.vignettes, "/", n.vignettes, "] Done appending vignette: ", vignette.name, " from ", package, sep=""), stdout())
#     }
#     
#     write(rdt.cmd.tail, instrumented.code.path, append=TRUE)
#     instrumented.vignette.paths[[i.packages]] <- c(package, "all_vignettes", instrumented.code.path)
#     
#     write(paste("[", i.packages, "/", n.packages, "] Done vignettes for package: ", package, sep=""), stdout())
#   }
#   
#   instrumented.vignette.paths
# }

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

#run <- function(..., separately=TRUE) 
#  execute.external.programs((if(separately) instrument.vignettes else instrument.and.aggregate.vignettes)(list(...)))
  
if (length(cfg$args) > 0)
  execute.external.programs(instrument.vignettes(packages=cfg$args), new.process = FALSE)
