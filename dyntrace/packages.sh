#!/bin/bash

# $1 is the R executable used for dyntracing
# $2 is the directory in which the dyntraces will be serialized.
# $3 is the number of separate parallel R processes that will be used to trace.
# $4-... packages to run tracing on, if not present then read from $PACKAGES_FILE
N_PROCESSES=1
if [ -n "$3" ]
then
    N_PROCESSES="$3"
fi

SCRIPT_DIR=$(cd $(dirname "$0"); pwd)
FILE=$SCRIPT_DIR/vignettes.R
OUTPUT_DIR=$(cd $2; pwd)/`date +"%Y-%m-%d-%H-%M-%S"`
DATA_DIR=$OUTPUT_DIR/data/
LOGS_DIR=$OUTPUT_DIR/logs/
PACKAGE_FILE=$SCRIPT_DIR/packages.csv
DYNTRACED_PACKAGE_FILE=$OUTPUT_DIR/packages.txt
mkdir -p $DATA_DIR
mkdir -p $LOGS_DIR
echo > $DYNTRACED_PACKAGE_FILE 

CMD="$1 --slave --no-restore --file=$FILE --args --output-dir=${OUTPUT_DIR}"

export R_COMPILE_PKGS=1
export R_DISABLE_BYTECODE=0
export R_ENABLE_JIT=3
export R_KEEP_PKG_SOURCE=yes
export RDT_COMPILE_VIGNETTE=false

PACKAGES=

if $RDT_COMPILE_VIGNETTE
then 
    CMD="$CMD --compile"        
fi    

shift 3
if [ $# -gt 0 ]
then
    PACKAGES="$@"
else
    PACKAGES=$(cat dyntrace/packages.csv | grep -v '^#' | grep -v '^$' | cut -f 1 -d';' | xargs echo)
fi

echo R_COMPILE_PKGS=$R_COMPILE_PKGS
echo R_DISABLE_BYTECODE=$R_DISABLE_BYTECODE
echo R_ENABLE_JIT=$R_ENABLE_JIT
echo R_KEEP_PKG_SOURCE=$R_KEEP_PKG_SOURCE
echo RDT_COMPILE_VIGNETTE=$RDT_COMPILE_VIGNETTE
echo N_PROCESSES=$N_PROCESSES
echo R_LIBS=$R_LIBS
echo Tracing packages: $PACKAGES

n_packages=`echo $PACKAGES | tr ' ' '\n' | wc -l`
slice_size=$(($n_packages / $N_PROCESSES + 1))

function trace {
    process=$1; shift
    echo "[$process] Slice: $@"
    for package in $@
    do 
        echo "[$process] Tracing $package ($CMD)"
        time $CMD $package 2>&1 | tee -a "${LOGS_DIR}/${package}.log" 
        echo "$package" >> $DYNTRACED_PACKAGE_FILE
        echo "[$process] Done tracing $package ($CMD)"
    done
}

for process in `seq 1 $N_PROCESSES`
do    
    slice=`echo $PACKAGES | tr ' ' '\n' | \
           tail -n +$((($process - 1)* $slice_size + 1)) | \
           head -n $slice_size`

    if [ $N_PROCESSES -le 1 ]
    then
        trace $process $slice
    else
        trace $process $slice &
        PIDS
    fi
    
done
