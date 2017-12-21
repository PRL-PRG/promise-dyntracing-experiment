#!/bin/bash

for f in "$@" #/home/kondziu/workspace/R-dyntrace/data/ELE-2/*.sqlite
do
    export CSV_DIR="`dirname $f`/../csv"/`basename $f .sqlite`	
    echo Extracting aggregate CSVs from $f to $CSV_DIR
    Rscript graphs/generate_data.R "$f"
    #R_LIBS=~/R/installed/ ../R-dyntrace/bin/Rscript graphs/generate_data.R "$f"
done

echo DONE

