#!/bin/bash

for f in "$@" #/home/kondziu/workspace/R-dyntrace/data/ELE-2/*.sqlite
do
    export CSV_DIR="`dirname $f`/../csv"	
    echo CSVifying $f to $CSV_DIR 	
    Rscript graphs/generate_data.R "$f"
done

echo DONE

