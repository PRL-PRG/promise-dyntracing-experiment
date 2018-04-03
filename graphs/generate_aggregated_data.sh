#!/bin/bash

export CSV_ROOT_DIR="$1/csv"
export DATA_ROOT_DIR="$1/data"
mkdir -p "$CSV_ROOT_DIR"

function_db="$1/data/_all/functions.sqlite"
export CSV_DIR="$CSV_ROOT_DIR/$3/"

echo Extracting preaggregated function data into CSV from $function_db to $CSV_DIR
Rscript graphs/generate_function_csv.R "$function_db" 

echo DONE

