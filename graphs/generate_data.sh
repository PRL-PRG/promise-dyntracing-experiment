!/bin/bash

for f in "$@" #/home/kondziu/workspace/R-dyntrace/data/ELE-2/*.sqlite
do 
    echo CSVifying $f	
    CSV_DIR="$f/csv" Rscript graphs/generate_data.R "$f"
done

echo DONE

