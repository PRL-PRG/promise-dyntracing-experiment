#!/usr/bin/env bash

OUTPUT_DIRNAME=promise-macro-usage
mkdir -p $OUTPUT_DIRNAME
MACROS=( PRVALUE PREXPR PRCODE PRENV )
for srcfile in $1/*.tar.gz; do
    printf "Processing $srcfile\n"
    cp $srcfile $OUTPUT_DIRNAME
    printf "Copied $srcfile to $OUTPUT_DIRNAME\n"
    base=$(basename $srcfile)
    destfile=$OUTPUT_DIRNAME/$base
    tar -xzf $destfile --directory=$OUTPUT_DIRNAME
    untardir=$(echo "${destfile::-7}" | sed -E -e 's/_[0123456789.-]+//')
    printf "Extracted $destfile to $untardir\n"
    for macro in "${MACROS[@]}"
    do
        grep -r "$macro" $untardir >> $OUTPUT_DIRNAME/${macro}.txt
    done
    rm -rf $untardir
    rm -rf $destfile
done
