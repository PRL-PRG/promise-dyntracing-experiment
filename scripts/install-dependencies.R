dependencies <- c('gdata', 'optparse', 'tidyverse',
                  'stringi', 'ggthemes', 'scales',
                  'crayon', 'magrittr', 'lubridate',
                  'broom', 'stringr', 'hashmap',
                  'gridExtra', 'kableExtra', 'png',
                  'ggplot2', 'knitr', 'bookdown',
                  'RSQLite', 'dbplyr', 'DT', 'tidyr',
                  'dplyr', 'ggsci', 'rvest', 'httr',
                  'progress', 'htmltidy', 'data.table',
                  'glue', 'readr', 'fs', 'data.table',
                  'devtools', 'roxygen2')

packages_to_install <- setdiff(dependencies, installed.packages())

install.packages(packages_to_install,
                 repos='http://cran.us.r-project.org',
                 ##Ncpus=20,                           # run in parallel using 20 cores
                 ##keep_outputs=T,                     # keeps outputs in ".out" files in current directory
                 INSTALL_opts=c(
                 "--byte-compile",                     # byte-compile packages
                 "--example",                          # extract and keep examples
                 "--install-tests",                    # copy and retain test directory for the package
                 "--with-keep.source",                 # keep line numbers
                 "--no-multiarch"))#,
                 #dependencies = c("Depends",
                 #                 "Imports",
                 #                 "LinkingTo",
                 #                 "Suggests",
                 #                 "Enhances"))
