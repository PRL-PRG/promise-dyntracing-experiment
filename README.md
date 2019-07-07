# Study of Laziness in R

This project uses [promisedyntracer](https://github.com/PRL-PRG/promisedyntracer "promisedyntracer") and [R-dyntrace](https://github.com/PRL-PRG/R-dyntrace "R-dyntrace") to generate, analyze and report laziness in R.

## Dependencies

The pipeline requires the following linux programs:
- xvfb-run
- time
- unbuffer
- tee
- parallel
- xdg-open
- zstd
- openssl

To install these programs on Ubuntu variants, run the following command:
```shell
$ sudo apt install xvfb unbuffer parallel expect xdg-utils libzstd-dev libssl-dev
```

## Administration

### Setup Repository

The following commands download and install all R packages from CRAN and Bioconductor.

```shell
$ make setup-repository PACKAGE_SETUP_REPOSITORIES=--setup-cran PACKAGE_SETUP_NCPUS=4
$ make setup-repository PACKAGE_SETUP_REPOSITORIES=--setup-bioc PACKAGE_SETUP_NCPUS=4
```

### Mirror Repository

The following command creates a local mirror of CRAN and BIOCONDUCTOR and extracts sources from compressed package files.

```shell
$ make mirror-repository
```

The following commands execute the corresponding stages of the analysis pipeline.
They have to be executed in order, intermediate stages read the result of their previous stage as input.

### Trace

```shell
$ make trace-ast CORPUS_FILEPATH=corpus/test.txt PARALLEL_JOB_COUNT=8 COMPRESSION_LEVEL=3 BINARY=--binary
```

### Prescan

```shell
$ make prescan-analysis TRACE_DIRPATH=latest
```

### Reduce

```shell
$ make reduce-analysis TRACE_DIRPATH=latest PARALLEL_JOB_COUNT=8 BINARY=--binary COMPRESSION_LEVEL=3 ANALYSIS=functions
```

### Scan

```shell
$ make scan-analyses TRACE_DIRPATH=latest
```

### Combine

```shell
$ make combine-analysis TRACE_DIRPATH=latest PARALLEL_JOB_COUNT=8 BINARY=--binary COMPRESSION_LEVEL=3 COMBINE_COUNT=10000 ANALYSIS=functions
```

### Merge

```shell
$ make merge-analysis TRACE_DIRPATH=latest BINARY=--binary COMPRESSION_LEVEL=3 ANALYSIS=functions
```

### Summarize

```shell
$ make summarize-analysis TRACE_DIRPATH=latest BINARY=--binary COMPRESSION_LEVEL=3 ANALYSIS=functions
```

### Report

```shell
$ make report-analysis TRACE_DIRPATH=latest BINARY=--binary COMPRESSION_LEVEL=3
```
