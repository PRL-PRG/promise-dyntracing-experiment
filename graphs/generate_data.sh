#!/bin/bash
PROCESSES=$1; shift 1
case $PROCESSES in
    [0-9]|[1-9][0-9]) ;;
    *) echo "Number of processes is not a number." >&2; exit 1;;
esac
echo Extracting CSVs from $1 using $PROCESSES processes
CSV_ROOT_DIR="$1/csv"
for db in "$1"/data/*.sqlite
do
    package=$(basename "$db" .sqlite)
    echo "${db}:${CSV_ROOT_DIR}/$package"     
done | xargs -P${PROCESSES} -I{} Rscript graphs/generate_csvs.R {}
echo DONE
