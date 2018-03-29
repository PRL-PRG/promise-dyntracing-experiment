#!/bin/bash
export R_LIBS=     #/home/kondziu/R_LIBS/
export DATA_DIR=/mnt/ssd/data/kondziu/
export PROCESSES=8

export DATA_DIR="$DATA_DIR/`ls $DATA_DIR -t | grep ....-..-..-..-..-..$ | head -1`"
export LOG_FILE="$DATA_DIR/processing.log"
export ANALYSIS_LOG_DIR="$DATA_DIR/logs/analysis/"

echo "Check package health and clean" | tee -a $LOG_FILE
make check PROCESSES=$PROCESSES DATA_DIR="$DATA_DIR"
make clean PROCESSES=$PROCESSES DATA_DIR="$DATA_DIR"
echo "Done checking package health and clean" | tee -a $LOG_FILE

echo "Starting individual analyses" | tee -a $LOG_FILE
echo -e "analyze-function-force\nanalyze-promise-lifespan\nanalyze-argument-promise-mode\nanalyze-promise-memory-usage\nanalyze-environment\nanalyze-position-evaluation-mode" | \
xargs --max-procs 8 -I{} ./run-one-analysis.sh {}
echo "Done with individual analyses" | tee -a $LOG_FILE

start=`date +%s`
echo "Starting CSV generation for bulk analyses" | tee -a $LOG_FILE
make csvs PROCESSES=$PROCESSES DATA_DIR="$DATA_DIR"
end=`date +%s`
echo "Done with CSV generation for bulk analyses in" $((($end - $start)/60/60)) "hours" | tee -a $LOG_FILE

start=`date +%s`
echo "Starting CSV aggregation for bulk analyses" | tee -a $LOG_FILE
make aggregate-csvs PROCESSES=$PROCESSES DATA_DIR="$DATA_DIR"
end=`date +%s`
echo Done with CSV aggregation for bulk analyses in" $((($end - $start)/60/60)) "hours" | tee -a $LOG_FILE

echo "Starting report for bulk analyses" | tee -a $LOG_FILE
make aggregate-csvs PROCESSES=$PROCESSES DATA_DIR="$DATA_DIR"
echo "Done with report for bulk analyses" | tee -a $LOG_FILE

start=`date +%s`
echo "Compute inference" | tee -a $LOG_FILE
make compute-interference PROCESSES=$PROCESSES DATA_DIR="$DATA_DIR"
end=`date +%s`
echo "Done compute inference in" $((($end - $start)/60/60)) "hours"| tee -a $LOG_FILE

start=`date +%s`
echo "Analyze inference" | tee -a $LOG_FILE
make analyze-interference PROCESSES=$PROCESSES DATA_DIR="$DATA_DIR"
end=`date +%s`
echo "Done analyze inference in" $((($end - $start)/60/60)) "hours" | tee -a $LOG_FILE

start=`date +%s`
echo "Analyze side effects" | tee -a $LOG_FILE
make analyze-side-effects PROCESSES=$PROCESSES DATA_DIR="$DATA_DIR"
end=`date +%s`
echo "Done analyze side effects in" $((($end - $start)/60/60)) "hours" | tee -a $LOG_FILE

echo "Make analysis book" | tee -a $LOG_FILE
make analysis-book PROCESSES=$PROCESSES DATA_DIR="$DATA_DIR"
echo "Done with analysis book" | tee -a $LOG_FILE
