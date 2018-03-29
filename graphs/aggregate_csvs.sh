#!/bin/bash
LOG=csv-health.log

if (( $# < 2 ))
then
    echo "Pass some dirs as arguments to aggregate: 1 output dir, 2-... input dirs"
    exit 1
fi

output_dir="$1"; shift
mkdir -p "$output_dir"

if [ ! -d "$output_dir" ]
then
    echo "Cannot create directory $output_dir"
    exit 2
fi

function precheck {
    all_files_exist=true
}

function check {
    if [ -f "$1" ] 
    then
        return 0
    else    
        echo missing "$1" >&2 
        all_files_exist=false
        return 1
    fi
}

function everything_ok {
    $all_files_exist && return 0 || return 1
}

echo -n > $LOG
for dir in "$@"
do
    if (( `ls "$dir" | grep '.csv$' | wc -l` == 0 ))
    then
        echo $dir BAD >> $LOG
        continue 
    fi

    precheck
    check "$dir/metadata.csv"
    check "$dir/basic_info.csv"
    check "$dir/actual_distances.csv"
    check "$dir/call_strictness_by_type.csv"
    check "$dir/call_strictness_rate.csv"
    check "$dir/call_strictness_ratio.csv"
    check "$dir/call_strictness.csv"
    check "$dir/call_types.csv"
    check "$dir/compiled_calls.csv"
    check "$dir/compiled_functions.csv"
    check "$dir/evaluation_order.csv"
    check "$dir/forces_by_type.csv"
    check "$dir/forces.csv"
    check "$dir/function_strictness_by_type.csv"
    check "$dir/function_strictness_rate.csv"
    check "$dir/function_strictness.csv"
    check "$dir/function_types.csv"
    check "$dir/fuzzy_forces.csv"
    check "$dir/promise_code_to_return_types.csv"
    check "$dir/promise_evaluations.csv"
    check "$dir/promise_full_types.csv"
    check "$dir/promise_types.csv"
    check "$dir/promises_forced_by_another_promise.csv"
    check "$dir/promises_forcing_other_promises.csv"
    check "$dir/return_types.csv"
    check "$dir/specific_calls.csv"
    if everything_ok 
    then    
	echo $dir GOOD >> $LOG 
    else 
        echo $dir BAD >> $LOG
	continue
    fi

    for file in "$dir"/*.csv
    do
        output_file="$output_dir"/`basename "$file"`
        if [ -e "$output_file" ]
        then
            echo "Appending data from $file to $output_file"
            <"$file" tail -n +2 >>"$output_file"
        else
            echo "Copying data from $file to $output_file"
            cp "$file" "$output_file"
        fi
    done
done
