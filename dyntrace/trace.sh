#!/bin/bash

if [ -e $STOP_TRACING ]
then
    echo "Detected stop tracing signal at $STOP_TRACING, stopping."
    exit 
fi

echo "Tracing $1"
echo $CMD $1 2>&1 | tee -a "${LOGS_DIR}/${1}.log"
     $CMD $1 2>&1 | tee -a "${LOGS_DIR}/${1}.log"
echo "$1" >> $ALREADY_TRACED_PACKAGES_LIST
echo "Done tracing $1"
