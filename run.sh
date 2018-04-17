#!/bin/bash
#
# 

# Parse this very file and print the entire first comment as usage information.
usage () {
    gawk '/^#/ && NR>1 {sub(/^#[ \t]?/,""); print; next} NR>1 {exit}' "$0"
}

# Helper function to print to stderr.
syserr() {
    echo $@ >&2
}

# Specification of parse options.
options=$(getopt -odpfhrstaPcCU \
    --long output-dir:,packages-from-file:,packages:,top:,randomize,sort-by-size,trace,analyze::,processes:,copy-traces-to:,compress,uncompress,rlibs:\
    -n $0 -- "$@")

# Stop if optparse encountered a problem.
optparse_error_code=$?
if [ $optparse_error_code != 0 ]; then 
    syserr "Parsing inexplicably failed (optparse error $optparse_error_code)"
    exit 1 
fi 

# Default settings
TRACE=false
ANALYZE=false
TOP=false
SORT_BY_SIZE=false
RANDOMIZE=false
COPY_TRACES=false
COMPRESS_TRACES=false
UNCOMPRESS_TRACES=false
PROCESSES=1

R_LIBS=/home/kondziu/R_LIBS
OUTPUT_DIR=
ARCHIVE_DIR=

PACKAGES=

# We crawl the analysis directory for all executable R sripts to use as the default.
ANALYSES=`find analysis -name '*.R' -executable | xargs -I{} basename {} .R | tr '\n' ' '`

# Set the parsed command options and interpret the settings.
eval set -- "$options" 
while true  
do 
    case "$1" in
    --rlibs)
        R_LIBS=$2
        shift 2;;
    -P|--processes)
        PROCESSES=$2
        shift 2;;
    -t|--trace) 
        TRACE=true
        shift 1;;
    -a|--analyze) 
        ANALYZE=true
        echo $1 x $2 x
        if [ -z "$2" ]; then
            echo $2
            # If analyses were not specified, then we use the default value
            shift 2        
        else
            echo 3
            # Otherwise we take in the comma separated list and convert
            # to a space-separated list.
            ANALYSES=`echo "$2" | tr , \ `
            shift 2
        fi;;
    -p|--packages) 
        # Read a comma-delimited list of packages as new-line delimited list.
        PACKAGES=`echo "$2" | tr , '\n' `
        shift 2;;
    -f|--packages-from-dir) 
        # Read a list of packages from a file. The file is assumed to
        # possibly have comments and other information after semicolons.
        PACKAGES=`cat "$2" | grep -v '^#' | grep . | cut -f 1 -d \;`
        shift 2;;        
    -h|--top) TOP=true; TOP_N=$2; shift 2;;
    -r|--randomize) RANDOMIZE=true; shift 1;;
    -s|--sort-by-size) SORT_BY_SIZE=true; shift 1;;
    -o|--output-dir)
        OUTPUT_DIR="$2"
        if [ ! -d "$OUTPUT_DIR" ]; then
            mkdir -p "$OUTPUT_DIR"
            if [ $? -ne 0 ]; then  
                syserr "Output dir $OUTPUT_DIR cannot be created." 
                exit 1
            fi
        fi
        shift 2;;
    -c|--copy-traces-to) 
        COPY_TRACES=true
        ARCHIVE_DIR="$2"
        if [ ! -d "$ARCHIVE_DIR" ]; then
            mkdir -p "$ARCHIVE_DIR"
            if [ $? -ne 0 ]; then
                syserr "Archive dir $ARCHIVE_DIR cannot be created."
                exit 1
            fi
        fi
        shift 2;;
    -C|--compress) COMPRESS_TRACES=true; shift 1;;
    -U|--uncompress) UNCOMPRESS_TRACES=true; shift 1;;
    --) shift; break;; 
    *) syserr "Unknown option $1"; usage; exit 3;; 
    esac 
done 

# Sort out the package list: clip and sort.
if $RANDOMIZE; then 
    PACKAGES=`echo $PACKAGES | sort -R`
fi
if $SORT_BY_SIZE; then
    PACKAGES=`echo $PACKAGES | sort` # TODO
fi
if $TOP; then
    PACKAGES=`echo $PACKAGES | head -n $TOP_N`
fi

# Export variables needed in run_one.sh
export TRACE
export ANALYZE
export ANALYSES
export COPY_TRACES
export COMPRESS_TRACES
export UNCOMPRESS_TRACES
export OUTPUT_DIR
export ARCHIVE_DIR
export R_LIBS

# Set R environment variables (if not set)
if [ -z $R_COMPILE_PKG ]; then export R_COMPILE_PKGS=1; fi
if [ -z $R_DISABLE_BYTECODE ]; then export R_DISABLE_BYTECODE=1; fi
if [ -z $R_ENABLE_JIT ]; then export R_ENABLE_JIT=0; fi
if [ -z $R_KEEP_PKG_SOURCE ]; then export R_KEEP_PKG_SOURCE=yes; fi
if [ -z $RDT_COMPILE_VIGNETTE ]; then export RDT_COMPILE_VIGNETTE=false; fi

# We start running things now.
echo "$PACKAGES" | xargs -P $PROCESSES -I{} ./run_one.sh {}
