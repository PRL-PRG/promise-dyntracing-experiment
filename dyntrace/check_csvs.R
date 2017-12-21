#/usr/bin/Rscript

f <- file("stdin")
open(f)

csv_dir <- commandArgs()[[2]]
csv <- list.files(csv_dir, pattern="basic_info.csv")

while(length(line <- readLines(f,n=1)) > 0) {
  # first: package, second: vignette, third: success
  columns <- strsplit(gsub(" +", "", line), fixed=" ")
  
  is_in_csv <- read.table(csv, header=TRUE, sep=";", quote="\"")
  
  write(
    paste(format(columns[[1]][1], width=20), 
          format(columns[[1]][2], width=50), 
          format(is_in_csv, width=5),
        sep=" "), 
    stdout())  
}