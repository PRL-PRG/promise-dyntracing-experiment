#!/bin/bash

if [ -e $STOP_TRACING ]
then
    echo "Detected stop tracing signal at $STOP_TRACING, stopping $1."
    exit 
fi

disk_size=`df --total . | tr -s ' ' | cut -f4 -d' ' | tail -n 1`
if [ $disk_size -lt "$MINIMUM_DISK_SIZE" ]
then
    echo "Available disk space ($disk_size) below threshold ($MINIMUM_DISK_SIZE), stopping $1."
    exit
fi

echo "Tracing $1"
echo $CMD $1 2>&1 | tee -a "${LOGS_DIR}/${1}.log"
     $CMD $1 2>&1 | tee -a "${LOGS_DIR}/${1}.log"
echo "$1" >> $ALREADY_TRACED_PACKAGES_LIST
echo "Done tracing $1"
