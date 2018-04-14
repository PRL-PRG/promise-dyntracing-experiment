#!/bin/bash
start=`date +%s`
echo "Starting $1" | tee -a $LOG_FILE
make "$1" DATA_DIR="$DATA_DIR" | tee "$ANALYSIS_LOG_DIR/$1.log"
end=`date +%s`
echo "Done $1 in" $((($end - $start)/60/60)) "hours" | tee -a $LOG_FILE    
