#!/usr/bin/env Rscript

source("analysis/utils.R")
source("analysis/analysis.R")

library(dplyr)

# expects a certain set of functions

# MAP
# for every DB file
analyze_database <- function(database_file_path) {
  components<-stringr::str_split(x, "-", 2)[[1]]
  
  list(general=data.frame(db=database_file_path, 
                          package = components[1],
                          vignette = components[2],
                          functions = functions %>% count %>% pull(n),
                          calls = calls %>% count %>% pull(n),
                          promises = promises %>% count %>% pull(n),
                          promise_forces = promise_evaluations %>% filter(event_type == 15) %>% count %>% pull(n),
                          promise_lookups = promise_evaluations %>% filter(event_type == 0) %>% count %>% pull(n)))
}

# combine dataframes from analyzeDatabase
# kind of like fold
# returns a list that has the same shape as analyzeDatabase
combine_analyses <- function(acc, element) {
  for(name in names(acc)) {    
    acc[[name]] = bind_rows(acc[[name]], element[[name]])  
  }  
  acc
}

# REDUCE
# runs analyses on all the data from all the vignettes
# analyses is the list form combine analyses
# returns the summarized data that undergoes visualization
summarize_analyses <- function(analyses) {
  list(data.frame(packages = analyses %>% pull(package) %>% unique %>% length,
                  vignette = analyses %>% pull(vignette) %>% unique %>% length,
                  functions = analyses %>% pull(functions) %>% sum,
                  calls = analyses %>% pull(calls) %>% sum,
                  promises = analyses %>% pull(promises) %>% sum,
                  promise_forces = analyses %>% pull(promise_forces) %>% sum,
                  promise_lookups = analyses %>% pull(promise_lookups) %>% sum))
}

#return a list of ggplot2 objects
# visualize_analyses <- function(analyses) {
#   # create factors with value labels
#    mtcars$gear <- factor(mtcars$gear,levels=c(3,4,5),
#    labels=c("3gears","4gears","5gears"))
#    mtcars$am <- factor(mtcars$am,levels=c(0,1),
#    labels=c("Automatic","Manual"))
#    mtcars$cyl <- factor(mtcars$cyl,levels=c(4,6,8),
#    labels=c("4cyl","6cyl","8cyl")) 
# 
#    # Kernel density plots for mpg
#    # grouped by number of gears (indicated by color)
#    plot <- qplot(mpg, data=mtcars, geom="density", fill=gear, alpha=I(.5),
#    main="Distribution of Gas Milage", xlab="Miles Per Gallon",
#    ylab="Density")
# 
#    list(abcd=plot)
# }


main <- function() {  
  drive_analysis("general-info",
                 analyze_database,
                 combine_analyses,
                 summarize_analyses, 
                 export_as_tables)  # exports CSV files automatically
                 #import_as_tables,  # (re-)imports data from CSV files
                 #visualize_analyses,
                 #export_as_images)
}

main()




