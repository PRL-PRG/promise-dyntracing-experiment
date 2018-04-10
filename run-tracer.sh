#!/bin/bash
export R_LIBS=/home/kondziu/R_LIBS/
export DATA_DIR=/mnt/ssd/data/kondziu/
export PROCESSES=4

start=`date +%s`

make trace PROCESSES=$PROCESSES DATA_DIR=$DATA_DIR PACKAGES="`cat /mnt/ssd/data/kondziu/2018-03-29-19-15-34/same_packages`"

# FIFTY_PACKAGES=`cat dyntrace/packages.csv | cut -f1 -d\; | head -n 50 | tr '\n' ' '`
# make trace PROCESSES=$PROCESSES DATA_DIR="$DATA_DIR" PACKAGES="$FIFTY_PACKAGES"

end=`date +%s`

DATA_DIR="$DATA_DIR/`ls $DATA_DIR -t | grep ....-..-..-..-..-..$ | head -1`"
LOG_FILE="$DATA_DIR/processing.log"

echo "Done tracing in" $((($end - $start)/60/60)) "hours" | tee $LOG_FILE
