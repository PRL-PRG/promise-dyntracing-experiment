#!/usr/bin/env Rscript

source("analysis/utils.R")
source("analysis/analysis.R")

library(dplyr)

# expects a certain set of functions

# MAP
# for every DB file
analyze_database <- function(database_file_path) {
  components <- stringr::str_split(
    basename(tools::file_path_sans_ext(database_file_path)), "-", 2)[[1]]
  
  db <- src_sqlite(database_file_path)
  jumps <- db %>% tbl("jumps")
  
  list(call_depth=(jumps %>% group_by(call_depth) %>% count %>% as.data.frame), #%>% mutate(db=database_file_path, package = components[1])
       promise_depth=(jumps %>% group_by(promise_depth) %>% count %>% as.data.frame),
       depth=(jumps %>% mutate(depth=call_depth + promise_depth) %>% group_by(call_depth) %>% count %>% as.data.frame),
       restarts=(jumps %>% summarise(restarts=sum(restarts), total=n()) %>% as.data.frame))
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
  list(call_depth=(analyses$call_depth %>% group_by(call_depth) %>% summarise(n=sum(n))),
       promise_depth=(analyses$promise_depth %>% group_by(promise_depth) %>% summarise(n=sum(n))),
       depth=(analyses$depth %>% group_by(depth) %>% summarise(n=sum(n))),
       restarts(analyses$restarts %>% summarise(restarts=sum(restarts), total=sum(total))))
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




