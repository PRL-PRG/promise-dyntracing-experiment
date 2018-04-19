#!/bin/bash

DATA_DIR=$OUTPUT_DIR/data
mkdir -p $DATA_DIR

LOGS_DIR=$OUTPUT_DIR/logs/
mkdir -p $LOGS_DIR

elapsed_time () {
    echo $((($2 - $1)/60/60)) "hours"  \
         $((($2 - $1)%3600/60)) "minutes and" \
         $((($2 - $1)%60)) "seconds"
}

# Unsetting R_LIBS so the analysis doesn't use instrumented libraries.
export R_LIBS=
if $SUMMARIZE; then
    echo "Summarizing results using $ANALYSES" | \
    tee -a "${LOGS_DIR}/_time.log"
    ANALYSIS_START=`date +%s`

    # Run individual analyses in n threads.
    echo $ANALYSES | xargs -P $PROCESSES -I{} \
        make analyze ANALYSIS=${} DATA_DIR="$OUTPUT_DIR" STAGE=analyze 2>&1 | \
             tee -a "${LOGS_DIR}/_summary_{}.log"

    ANALYSIS_END=`date +%s`
    echo "Done summarizing results using $ANALYSES in" \
         `elapsed_time $ANALYSIS_START $ANALYSIS_END` | \
         tee -a "${LOGS_DIR}/_time.log"
fi

