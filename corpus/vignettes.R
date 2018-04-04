library(tibble)
library(dplyr)

available <- available.packages(contriburl = "file:///data/CRAN/contrib")[,1]
installed <- installed.packages()[,1]
count_loc <- function(path) {
  if (file.exists(path))
    length(grepl("^[ \t\n]*#", grepl("[^ \t\n]", readLines(path)), invert=TRUE))
  else 0
}

get_vignettes <- function(package) {
  v <- vignette(package=package)$results
  x <- if (length(v) == 0) integer(0) else {
    paths <- paste0(v[,2], "/", v[,1], "/doc/", v[,3], ".R")
    sapply(paths, count_loc)
  }
  loc <- sum(x)
  tibble(package=package,
         vignettes=list(v[,3]), 
         n_vignettes=length(v[,3]),
         n_runnable_vignettes=sum(x > 0),
         loc=loc)
}

everything <- Reduce(rbind, Map(get_vignettes, installed))

summary <- tibble(available_packages=(available %>% length),
                  installed_packages=(everything %>% group_by(package) %>% count %>% pull(n) %>% sum),
                  packages_with_vignettes=(everything %>% filter(n_vignettes > 0) %>% group_by(package) %>% count %>% pull(n) %>% sum),
                  packages_with_runnable_vignettes=(everything %>% filter(loc > 0) %>% group_by(package) %>% count %>% pull(n) %>% sum),
                  number_of_vignettes=(everything %>% summarise(s=sum(n_vignettes)) %>% pull(s)),
                  number_of_runnable_vignettes=(everything %>% summarise(s=sum(n_runnable_vignettes)) %>% pull(s)),
                  total_lines_of_code=(everything %>% summarise(s=sum(loc)) %>% pull(s)))

options(tibble.width = Inf)
print(summary %>% as.data.frame)