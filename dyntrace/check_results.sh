#!/bin/bash

# $1 - directory containing the results (can be nested, i.e. enough to give 2017-05-15-.../ without specifying path to data/*.sqlite)

function get_status {
while read sqlite_file
do
    directory=`dirname $sqlite_file`
    base_file=`basename $sqlite_file .sqlite`
    ok=`[ -f $directory/$base_file.OK ] && echo TRUE || echo FALSE`
    size=`du -h $sqlite_file | cut -f 1`
    package=`echo $base_file | cut -f 1 -d-`
    vignette=`echo $base_file | cut -f 2 -d-`
    printf "%20s %50s %8s %5s\n"  $package $vignette $size $ok $csv

done
}

info=`find "$1" -iname '*.sqlite' | get_status | sort -k 1,4`
correct=`echo "$info" | tr -s ' ' |  cut -f 5 -d ' ' | grep TRUE | wc -l`
incorrect=`echo "$info" | tr -s ' ' |  cut -f 5 -d ' ' | grep FALSE | wc -l`
total=`echo "$info" | wc -l`

echo "$info" | tee "$1"/health.txt
printf "\n%42s  SUCCEEDED: %4s  FAILED: %4s  TOTAL: %4s\n" "" $correct $incorrect $total | tee -a "$1"/health.txt


