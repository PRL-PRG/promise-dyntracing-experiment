#!/bin/bash

mkdir /tmp/bad_sqlites
bad_sqlites=
for sqlite in $1/data/*.sqlite
do
    echo $ok
    if sqlite3 $sqlite "select * from functions;" > /dev/null #"pragma integrity_check;" > /dev/null
    then
	echo "$sqlite passed" >&2 
    else	
        echo "$sqlite failed" >&2	 
        bad_sqlites="$good_sqlites$sqlite "
        mv -v $sqlite /tmp/bad_sqlites	
    fi
done
echo $bad_sqlites
#generate_sql "$1/data/*.sqlite" | sqlite3 "$OUTPUT_DB"

exit 1
