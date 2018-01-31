#!/bin/bash

function generate_sql {
    DATA_FILES=$@
    union=
    cleanup=
    
    echo ".echo on"
    echo ".bail on"
    echo
    
    echo "pragma journal_mode = off;"
    echo
    
    for file in $DATA_FILES
    do 
        package=$(basename "$file" .sqlite | tr '.-' '__')

        echo "attach '$file' as $package;"
        echo "create table temp_${package}_functions as select * from ${package}.functions;"
        echo "detach $package;"
        echo 

        if [ -n "$union" ]
        then
            union="$union\n  union"
        fi
        union="$union select * from temp_${package}_functions"
        
        if [ -n "$cleanup" ]
        then
            cleanup="$cleanup\n"
        fi
        cleanup="${cleanup}drop table temp_${package}_functions;"
    done
    
    echo "begin transaction;"

    echo -e "create table functions as select * from (\n       $union);"
    echo
    echo -e "$cleanup"
    
    echo "commit;"
}

INPUT_DBS="$1/data/*.sqlite"
OUTPUT_DIR="$1/data/_all"
OUTPUT_DB="$OUTPUT_DIR/functions.sqlite"

if [ $(ls $(dirname $INPUT_DBS) | wc -l) -eq 0 ]; then
    echo "No sqlite files supplied to concatenate. Qutting."
    exit 1
fi    

if mkdir -p $OUTPUT_DIR; then :
else    
    echo "Cannot create dir $OUTPUT_DIR. Quitting."
    exit 2
fi    

if [ -e $OUTPUT_DB ]; then
    echo "Warning: file $OUTPUT_DB already exists and will be truncated."
    rm -i "$OUTPUT_DB" || exit 3
fi    

generate_sql "$1/data/*.sqlite" | sqlite3 "$OUTPUT_DB"
