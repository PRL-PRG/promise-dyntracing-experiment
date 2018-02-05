#!/bin/bash

export CSV_ROOT_DIR="$1/csv"
export DATA_ROOT_DIR="$1/data"
mkdir -p "$CSV_ROOT_DIR"

for db in "$1"/data/*.sqlite
do
    package=$(basename "$db" .sqlite)
    export CSV_DIR="$CSV_ROOT_DIR/$package"
    mkdir -p "$CSV_DIR"

    Rscript graphs/generate_csvs.R "$db" "$CSV_DIR" \
        && echo "$package;TRUE" >> "$CSV_ROOT_DIR/health.csv" \
        || echo "$package;FALSE" >> "$CSV_ROOT_DIR/health.csv"    
done

echo DONE

