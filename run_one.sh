#!/bin/bash
PACKAGE="$1"
N_PROCESSES=1

DATA_DIR=$OUTPUT_DIR/data
mkdir -p $DATA_DIR

LOGS_DIR=$OUTPUT_DIR/logs/
mkdir -p $LOGS_DIR

elapsed_time () {
    echo $((($2 - $1)/60/60)) "hours"  \
         $((($2 - $1)%3600/60)) "minutes and" \
         $((($2 - $1)%60)) "seconds"
}

if $IMPORT_TRACES; then
    echo "Importing traces of $PACKAGE" from $ARCHIVE_DIR | tee -a "${LOGS_DIR}/_time.log"
    IMPORT_START=`date +%s`

    # Copy all files except OK files to archive.
    ls $ARCHIVE_DIR/$PACKAGE-* | xargs -I{} cp -v {} $DATA_DIR

    IMPORT_END=`date +%s`
    echo "Done importing traces results from $PACKAGE to $ARCHIVE_DIR in" \
         `elapsed_time $IMPORT_START $IMPORT_END` | \
         tee -a "${LOGS_DIR}/_time.log"

fi


echo "Processing $PACKAGE" | tee -a "${LOGS_DIR}/_time.log"
START=`date +%s`

if $UNCOMPRESS_TRACES; then
    echo "Uncompressing results from $PACKAGE" | tee -a "${LOGS_DIR}/_time.log"
    COMPRESS_START=`date +%s`

    # Uncompress all files except from this package.
    ls $DATA_DIR/$PACKAGE-*.gz | grep gz$ | xargs gunzip -v

    COMPRESS_END=`date +%s`
    echo "Done uncompressing results from $PACKAGE in" \
         `elapsed_time $COMPRESS_START $COMPRESS_END` | \
         tee -a "${LOGS_DIR}/_time.log"
fi


if $TRACE; then
    echo "Tracing $PACKAGE" | tee -a "${LOGS_DIR}/_time.log"
    TRACING_START=`date +%s`

    # Run the tracer.
    if $RDT_COMPILE_VIGNETTE; then compile="--compile"; fi
    if $DEBUG; then
    echo \
    ../R-dyntrace/bin/R $debug --slave --no-restore --file=dyntrace/vignettes.R \
                        --args --output-dir="${OUTPUT_DIR}" $compile \
                        $PACKAGE 2>&1 | tee -a "${LOGS_DIR}/${PACKAGE}.log"
    fi

    if $VIGNETTES; then vignettes=--vignettes; fi
    if $EXAMPLES; then examples=--examples; fi
    if $TESTS; then tests=--tests; fi

    ../R-dyntrace/bin/R $debug --slave --no-restore --file=dyntrace/trace.R \
                        --args --output-dir="${OUTPUT_DIR}" $compile \
                        $vignettes $examples $tests \
                        $PACKAGE 2>&1 | tee -a "${LOGS_DIR}/${PACKAGE}.log"

    TRACING_END=`date +%s`
    echo "Done tracing $PACKAGE in" \
         `elapsed_time $TRACING_START $TRACING_END` | \
         tee -a "${LOGS_DIR}/_time.log"

    # Add up the size of all databse files from vignettes, write it to a log.
    du -s $DATA_DIR/$PACKAGE::* | cut -f 1 | paste -sd+ | bc | \
        xargs -I{} echo $PACKAGE\;{} | tee -a $LOGS_DIR/_space.log | \
        cut -f 2 -d\; | echo Trace size: 

    # FIXME add indexes
fi


# Unsetting R_LIBS so the analysis doesn't use instrumented libraries.
export R_LIBS=
if $ANALYZE; then
    echo "Analyzing $PACKAGE using $ANALYSES" | tee -a "${LOGS_DIR}/_time.log"
    ANALYSIS_START=`date +%s`

    # Run individual analyses one-by-one.
    for analysis in $ANALYSES; do
        make analyze ANALYSIS=${analysis} \
                     DATA_DIR="$OUTPUT_DIR" \
                     STAGE=analyze 2>&1 | \
             tee -a "${LOGS_DIR}/${PACKAGE}.log"
    done

    ANALYSIS_END=`date +%s`
    echo "Done analyzing $PACKAGE using $ANALYSES in" \
         `elapsed_time $ANALYSIS_START $ANALYSIS_END` | \
         tee -a "${LOGS_DIR}/_time.log"
fi

if $COMPRESS_TRACES; then
    echo "Compressing results from $PACKAGE" | tee -a "${LOGS_DIR}/_time.log"
    COMPRESS_START=`date +%s`

    # Compress all files except OK files and except files that are already 
    # compressed.
    ls $DATA_DIR/$PACKAGE-* | grep -v OK$ | grep -v gz$ | xargs gzip -v

    COMPRESS_END=`date +%s`
    echo "Done compressing results from $PACKAGE in" \
         `elapsed_time $COMPRESS_START $COMPRESS_END` | \
         tee -a "${LOGS_DIR}/_time.log"
fi

if $COPY_TRACES || $MOVE_TRACES; then
    echo "Relocating traces of $PACKAGE" to $ARCHIVE_DIR | tee -a "${LOGS_DIR}/_time.log"
    COPY_START=`date +%s`

    # Copy all files except OK files to archive.
    ls $DATA_DIR/$PACKAGE-* | grep -v OK$ | if $COPY_TRACES; then xargs -I{} cp -v {} $ARCHIVE_DIR; else xargs -I{} mv -v {} $ARCHIVE_DIR; fi

    COPY_END=`date +%s`
    echo "Done relocating traces results from $PACKAGE to $ARCHIVE_DIR in" \
         `elapsed_time $COPY_START $COPY_END` | \
         tee -a "${LOGS_DIR}/_time.log"

fi

END=`date +%s`
echo "Done processing $PACKAGE in" \
     `elapsed_time $START $END` | \
     tee -a "${LOGS_DIR}/_time.log"

