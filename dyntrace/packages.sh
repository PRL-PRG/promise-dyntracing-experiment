#!/bin/bash

START_TIME=$(date +%s)
# $1 is the R executable used for dyntracing
# $2 is the directory in which the dyntraces will be serialized.
# $3 is the number of separate parallel R processes that will be used to trace.
# $4 is the mminimum disk size below which the tracer will stop
# $5-... packages to run tracing on, if not present then read from $PACKAGES_FILE
N_PROCESSES=1
if [ -n "$3" ]
then
    N_PROCESSES="$3"
fi

SCRIPT_DIR=$(cd $(dirname "$0"); pwd)

export OUTPUT_DIR=$(cd $2; pwd)/`date +"%Y-%m-%d-%H-%M-%S"`
export STOP_TRACING=STOP
export ALREADY_TRACED_PACKAGES_LIST=$OUTPUT_DIR/packages.txt
export LOGS_DIR=$OUTPUT_DIR/logs/
export MINIMUM_DISK_SIZE=$4

# Prepare files and directories
mkdir -p $OUTPUT_DIR/data
mkdir -p $LOGS_DIR
echo > $ALREADY_TRACED_PACKAGES_LIST
[ -e $STOP_TRACING ] && rm $STOP_TRACING

#  Prepare R commandline  magic spell
export CMD="$1 --slave --no-restore --file=${SCRIPT_DIR}/vignettes.R --args --output-dir=${OUTPUT_DIR} ${ENABLE_TRACE} ${ANALYSIS_SWITCH}"
if $RDT_COMPILE_VIGNETTE
then
    export CMD="$CMD --compile"
fi

# Package list
shift 4
if [ $# -gt 0 ]
then
    PACKAGES="$@"
else
    PACKAGES=$(cat "$SCRIPT_DIR/packages.csv" | grep -v '^#' | grep -v '^$' | cut -f 1 -d';' | xargs echo)
fi

# R environmental variables
export R_COMPILE_PKGS=1
export R_DISABLE_BYTECODE=1
export R_ENABLE_JIT=1
export R_KEEP_PKG_SOURCE=yes
export RDT_COMPILE_VIGNETTE=false

# Debug info
echo R_COMPILE_PKGS=$R_COMPILE_PKGS
echo R_DISABLE_BYTECODE=$R_DISABLE_BYTECODE
echo R_ENABLE_JIT=$R_ENABLE_JIT
echo R_KEEP_PKG_SOURCE=$R_KEEP_PKG_SOURCE
echo RDT_COMPILE_VIGNETTE=$RDT_COMPILE_VIGNETTE
echo N_PROCESSES=$N_PROCESSES
echo R_LIBS=$R_LIBS
echo Tracing packages: $PACKAGES

# Run everything potentially in parallel
echo $PACKAGES | tr ' ' '\n' | xargs -I{} -P${N_PROCESSES} dyntrace/trace.sh {}

END_TIME=$(date +%s)
DIFF_TIME=$(( $END_TIME - $START_TIME ))
echo "It took $DIFF_TIME seconds"
