#!/usr/bin/env bash

OUTPUT_DIRNAME=delayed-assign-usage
mkdir -p $OUTPUT_DIRNAME
for srcfile in $1/*.tar.gz; do
    printf "Processing $srcfile\n"
    cp $srcfile $OUTPUT_DIRNAME
    printf "Copied $srcfile to $OUTPUT_DIRNAME\n"
    base=$(basename $srcfile)
    destfile=$OUTPUT_DIRNAME/$base
    tar -xzf $destfile --directory=$OUTPUT_DIRNAME
    untardir=$(echo "${destfile::-7}" | sed -E -e 's/_[0123456789.-]+//')
    printf "Extracted $destfile to $untardir\n"
    grep -r "delayedAssign" $untardir >> $OUTPUT_DIRNAME/delayed-assign-usage.txt
    rm -rf $untardir
    rm -rf $destfile
done
