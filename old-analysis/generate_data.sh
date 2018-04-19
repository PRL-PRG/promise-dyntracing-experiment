#!/bin/bash
PROCESSES=$1; shift 1
case $PROCESSES in
    [0-9]|[1-9][0-9]) ;;
    *) echo "Number of processes is not a number." >&2; exit 1;;
esac
echo Extracting CSVs from $1 using $PROCESSES processes
CSV_ROOT_DIR="$1/csv"
for analysis in basic-info metadata promise_evaluations forces fuzzy_forces promise_evaluations promises_forced_by_another_promise promises_forcing_other_promises promise_types promise_full_types return_types promise_code_to_return_types forces_by_type actual_distances call_types compiled_calls function_types compiled_functions call_strictness call_strictness_rate call_strictness_ratio call_strictness_by_type function_strictness evaluation_order specific_calls
do	
    for db in "$1"/data/*.sqlite
    do
        package=$(basename "$db" .sqlite)
        echo "${analysis}:${db}:${CSV_ROOT_DIR}/$package"     
    done	
done | xargs -P${PROCESSES} -I{} Rscript graphs/generate_csvs.R {}
echo DONE
