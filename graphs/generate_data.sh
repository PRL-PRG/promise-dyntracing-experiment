#!/bin/bash

for f in "$@" #/home/kondziu/workspace/R-dyntrace/data/ELE-2/*.sqlite
do
    export CSV_DIR="`dirname $f`/../csv"/`basename $f .sqlite`	
    CSV_ROOT_DIR="`dirname $f`/../csv"
    echo Extracting aggregate CSVs from $f to $CSV_DIR
    mkdir -p $CSV_DIR
    Rscript graphs/generate_data.R "$f" \
        && echo ` basename "$f"`";TRUE" >> "$CSV_ROOT_DIR/health.csv" \
        || echo ` basename "$f"`";FALSE" >> "$CSV_ROOT_DIR/health.csv"

    #R_LIBS=~/R/installed/ ../R-dyntrace/bin/Rscript graphs/generate_data.R "$f"
done

echo DONE

