#!/bin/bash
PACKAGE="$1"
N_PROCESSES=1

DATA_DIR=$OUTPUT_DIR/data
mkdir -p $DATA_DIR

LOGS_DIR=$OUTPUT_DIR/logs/
mkdir -p $LOGS_DIR

elapsed_time () {
    echo $((($2 - $1)/60)) "minutes"
}

echo "Processing $PACKAGE" | tee -a "${LOGS_DIR}/_time.log"
START=`date +%s`

if $TRACE; then
    echo "Tracing $PACKAGE" | tee -a "${LOGS_DIR}/_time.log"
    TRACING_START=`date +%s`

    # Run the tracer.
    if $RDT_COMPILE_VIGNETTE; then compile="--compile"; fi
    ../R-dyntrace/bin/R --slave --no-restore --file=dyntrace/vignettes.R \
                        --args --output-dir="${OUTPUT_DIR}" $compile \
                        $PACKAGE 2>&1 | tee -a "${LOGS_DIR}/${PACKAGE}.log"

    TRACING_END=`date +%s`
    echo "Done tracing $PACKAGE in" \
         `elapsed_time $TRACING_START $TRACING_END` | \
         tee -a "${LOGS_DIR}/_time.log"

    # Add up the size of all databse files from vignettes, write it to a log.
    du -s $DATA_DIR/$PACKAGE-* | cut -f 1 | paste -sd+ | bc | \
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
        make analyze ANALYSIS=${analysis} DATA_DIR="$OUTPUT_DIR" STAGE=analyze 2>&1 | \
             tee -a "${LOGS_DIR}/${PACKAGE}.log"
    done

    ANALYSIS_END=`date +%s`
    echo "Done analyzing $PACKAGE using $ANALYSES in" \
         `elapsed_time $ANALYSIS_START $ANALYSIS_END` | \
         tee -a "${LOGS_DIR}/_time.log"
fi

#if $COMPRESS_TRACES; then
    
#fi

#if $COPY_TRACES; then

#fi

END=`date +%s`
echo "Done processing $PACKAGE in" \
     `elapsed_time $START $END` | \
     tee -a "${LOGS_DIR}/_time.log"

