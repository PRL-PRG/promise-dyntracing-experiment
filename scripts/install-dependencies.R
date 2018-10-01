dependencies <- c("gdata", "optparse", "tidyverse",
                  "stringi", "ggthemes", "scales",
                  "crayon", "magrittr", "lubridate",
                  "broom", "stringr", "hashmap",
                  "gridExtra", "kableExtra", "png",
                  "ggplot2", "knitr", "bookdown",
                  "RSQLite", "dbplyr", "DT", "tidyr",
                  "dplyr", "ggsci", "rvest", "httr",
                  "progress", "htmltidy", "data.table",
                  "glue", "readr", "fs", "data.table",
                  "devtools", "roxygen2", "shiny",
                  "shinyAce", "codetools")

packages_to_install <- setdiff(dependencies, installed.packages())

if (length(packages_to_install) != 0)
    install.packages(packages_to_install,
                     repos = "http://cran.us.r-project.org",
                     ##run in parallel using 20 cores
                     ##Ncpus=20,
                     ## keeps outputs in ".out" files in current directory
                     ##keep_outputs=T,
                     INSTALL_opts = c(
                         ## byte-compile packages
                         ##"--byte-compile",
                         ## extract and keep examples
                         "--example",
                         ## copy and retain test directory for the package
                         "--install-tests",
                         ## keep line numbers
                         "--with-keep.source",
                         "--no-multiarch"))#,
                     #dependencies = c("Depends",
                     #                 "Imports",
                     #                 "LinkingTo",
                     #                 "Suggests",
                     #                 "Enhances"))
