#!/bin/bash

# $1 - directory containing the results (can be nested, i.e. enough to give 2017-05-15-.../ without specifying path to data/*.sqlite)

rm -i $(find "$1" -iname '*.sqlite' | \
    while read sqlite_file
    do
        base_file=`basename $sqlite_file .sqlite`
        directory=`dirname $sqlite_file`
        ok=`[ -f $directory/$base_file.OK ] && echo TRUE || echo FALSE`
        [ $ok = FALSE ] && echo $sqlite_file
    done)
