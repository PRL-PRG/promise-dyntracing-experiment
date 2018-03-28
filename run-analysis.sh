#!/bin/bash
export R_LIBS=/home/kondziu/R_LIBS/
export DATA_DIR=/mnt/ssd/data/kondziu/
export PROCESSES=8

DATA_DIR="$DATA_DIR/`ls $DATA_DIR -t | grep ....-..-..-..-..-..$ | head -1`"
LOG_FILE="$DATA_DIR/processing.log"
ANALYSIS_LOG_DIR="$DATA_DIR/logs/analysis/"

function analyze_a_thing {
    echo "Starting $1" | tee -a $LOG_FILE
    make "$1" DATA_DIR="$DATA_DIR" | tee "$ANALYSIS_LOG_DIR/$1.log"
    echo "Done $1" | tee -a $LOG_FILE    
}

echo "Starting individual analyses" | tee -a $LOG_FILE
echo analyze-function-force \
     analyze-promise-lifespan \
     analyze-argument-promise-mode \     
     analyze-promise-memory-usage \
     analyze-environment \
     analyze-position-evaluation-mode \
xargs --max-procs 8 -I{} analyze_a_thing {}
echo "Done with individual analyses" | tee -a $LOG_FILE

start=`date +%s`
echo "Starting CSV generation for bulk analyses" | tee -a $LOG_FILE
make csvs PROCESSES=$PROCESSES DATA_DIR="$DATA_DIR"
end=`date +%s`
echo "Done with CSV generation for bulk analyses in" $((($end - $start)/1000/60/60)) "hours" | tee -a $LOG_FILE

start=`date +%s`
echo "Starting CSV aggregation for bulk analyses" | tee -a $LOG_FILE
make aggregate-csvs PROCESSES=$PROCESSES DATA_DIR="$DATA_DIR"
end=`date +%s`
echo Done with CSV aggregation for bulk analyses in" $((($end - $start)/1000/60/60)) "hours" | tee -a $LOG_FILE

echo "Starting report for bulk analyses" | tee -a $LOG_FILE
make aggregate-csvs PROCESSES=$PROCESSES DATA_DIR="$DATA_DIR"
echo "Done with report for bulk analyses" | tee -a $LOG_FILE

start=`date +%s`
echo "Compute inference" | tee -a $LOG_FILE
make compute-interference PROCESSES=$PROCESSES DATA_DIR="$DATA_DIR"
end=`date +%s`
echo "Done compute inference in" $((($end - $start)/1000/60/60)) "hours"| tee -a $LOG_FILE

start=`date +%s`
echo "Analyze inference" | tee -a $LOG_FILE
make analyze-interference PROCESSES=$PROCESSES DATA_DIR="$DATA_DIR"
end=`date +%s`
echo "Done analyze inference in" $((($end - $start)/1000/60/60)) "hours" | tee -a $LOG_FILE

start=`date +%s`
echo "Analyze side effects" | tee -a $LOG_FILE
make analyze-side-effects PROCESSES=$PROCESSES DATA_DIR="$DATA_DIR"
end=`date +%s`
echo "Done analyze side effects in" $((($end - $start)/1000/60/60)) "hours" | tee -a $LOG_FILE

echo "Make analysis book" | tee -a $LOG_FILE
make analysis-book PROCESSES=$PROCESSES DATA_DIR="$DATA_DIR"
echo "Done with analysis book" | tee -a $LOG_FILE
