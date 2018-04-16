#!/bin/bash
PACKAGE="$1"
N_PROCESSES=1

LOGS_DIR=$OUTPUT_DIR/logs/
mkdir -p $LOGS_DIR
mkdir -p $OUTPUT_DIR/data/

echo "Processing $PACKAGE" | tee -a "${LOGS_DIR}/_time.log"
STARTS=`date +%s`
if $TRACE; then
    echo "Tracing $PACKAGE" | tee -a "${LOGS_DIR}/_time.log"
    echo \
    ../R-dyntrace/bin/R --slave --no-restore --file=dyntrace/vignettes.R \
                        --args --output-dir="${OUTPUT_DIR}" $compile $PACKAGE

    if $RDT_COMPILE_VIGNETTE; then
       compile="--compile"
    fi

    TRACING_STARTS=`date +%s`
    ../R-dyntrace/bin/R --slave --no-restore --file=dyntrace/vignettes.R \
                        --args --output-dir="${OUTPUT_DIR}" $compile \
                        $PACKAGE 2>&1 | tee -a "${LOGS_DIR}/${PACKAGE}.log"
    TRACING_ENDS=`date +%s`

    echo "Done tracing $PACKAGE in" \
         $((($TRACING_ENDS - $TRACING_STARTS)/60)) "minutes" | \
         tee -a "${LOGS_DIR}/_time.log"

    for db in `ls $OUTPUT_DIR/data/$PACKAGE-*.sqlite`; do
        du -sh $db | tee -a $OUTPUT_DIR/logs/_space.log
    done

    # FIXME add indexes
    
fi

export R_LIBS=
print $ANALYSES
if $ANALYZE; then
    echo "Analyzing $PACKAGE" | tee -a "${LOGS_DIR}/_time.log"

    for analysis in "$ANALYSES"; do
        echo \
        make analyze ANALYSIS=${analysis} DATA_DIR="$OUTPUT_DIR" STAGE=analyze 

        make analyze ANALYSIS=${analysis} DATA_DIR="$OUTPUT_DIR" STAGE=analyze 2>&1 | \
             tee -a "${LOGS_DIR}/${PACKAGE}.log"
    done

    echo "Done analyzing $PACKAGE in" \
         $((($TRACING_ENDS - $TRACING_STARTS)/60)) "minutes" | \
         tee -a "${LOGS_DIR}/_time.log"
fi

#if $COMPRESS_TRACES; then
    
#fi

#if $COPY_TRACES; then

#fi

END=`date +%s`
echo "Done processing $PACKAGE in" \
     $((($TRACING_ENDS - $TRACING_STARTS)/60)) "minutes" | \
     tee -a "${LOGS_DIR}/_time.log"

