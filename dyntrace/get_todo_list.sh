#!/bin/bash

# $1 - directory containing the results (can be nested, i.e. enough to give 2017-05-15-.../ without specifying path to data/*.sqlite)


done=$(mktemp /tmp/rdt_done.XXXXXX)
all=$(mktemp /tmp/rdt_todo.XXXXXX)

find "$1" -iname '*.sqlite' | \
while read sqlite_file
do
    basename $sqlite_file .sqlite | cut -f 1 -d-        
done | \
sort > "$done"

<dyntrace/packages.csv cut -f 1 -d\; | sort > "$all"

comm -2 -3 "$all" "$done"

rm "$all" "$done"
