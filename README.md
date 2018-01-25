# Running Experiments

Scripts for collecting and processing data about promise execution in R

## Prerequisites

The following packages are necessary for running the experiments (in addition
to any traced packages): `optparse`, `stringr`, `hashmap`, `tidyr`, `dplyr`,
`gridExtra`, `kableExtra`, `png`, `scales`, `ggplot2`, `knitr`.

Installation script:

```r
install.packages(c("optparse", "stringr", "hashmap", 
                   "tidyr", "dplyr", "gridExtra", 
                    "kableExtra", "png", "scales", 
                    "ggplot2", "knitr"),
                 #repos='http://cran.us.r-project.org',
                 #Ncpus=20,                            # run in parallel using 20 cores
                 #keep_outputs=T,                      # keeps outputs in ".out" files in current directory
                 INSTALL_opts=c(
                 "--byte-compile",                     # byte-compile packages
                 "--example",                          # extract and keep examples
                 "--install-tests",                    # copy and retain test directory for the package
                 "--with-keep.source",                 # keep line numbers
                 "--no-multiarch"),
                 dependencies = c("Depends",
                                  "Imports",
                                  "LinkingTo",
                                  "Suggests",
                                  "Enhances"))
```

## Basic process

The basic process of tracing vignettes and processing the data is as follows. We describe the constitutent parts and various options below in more detail.

``` bash
export R_LIBS=$DATA_ROOT/R/CRAN/
make trace PROCESSES=1 DATA_DIR="$DATA_ROOT/R/traces/$HOSTNAME"                           # VERY LONG
NEW_RESULT_DIR=`ls $DATA_ROOT/R/traces/$HOSTNAME -t | grep ....-..-..-..-..-.. | head -1`
make check DATA_DIR="$DATA_ROOT/R/traces/$HOSTNAME/$NEW_RESULT_DIR"
make get-todo-packages DATA_DIR="$DATA_ROOT/R/traces/$HOSTNAME/$NEW_RESULT_DIR"
make clean DATA_DIR="$DATA_ROOT/R/traces/$HOSTNAME/$NEW_RESULT_DIR"
make csvs DATA_DIR="$DATA_ROOT/R/traces/$HOSTNAME/$NEW_RESULT_DIR"                        # LONG
make aggregate-csvs DATA_DIR="$DATA_ROOT/R/traces/$HOSTNAME/$NEW_RESULT_DIR"
make report DATA_DIR="$DATA_ROOT/R/traces/$HOSTNAME/$NEW_RESULT_DIR"                      # LONG
```

## Trace vignettes

Run the vignettes of of packages and produces an sqlite database with a lot of data after a lot of time. 

``` bash
export R_LIBS=$INSTALLED_R_PACKAGES
make trace PROCESSES=1 DATA_DIR="$DATA_ROOT/R/traces/$HOSTNAME/"
```

The list of packages is located at `dyntrace/packages.csv` and contains all packages that have runnable vignettes. One can also specify a different list as shown further below.

The R instance/tracer that is used by default is ../R-dyntrace/bin/R. A different oue can be specified as we show further below. The environmental variable `R_LIBS` should be set to the installation directory for the given tracer and all traced packages should be installed. If a package is not installed, the tracer will try installing it to `R_LIBS` before tracing.

`PROCESSES` specifies how many separate R instances should run in parallel. This can lead to disk contention and uses up more RAM during runtime, which can lead to some vignettes not being run.

`DATA_DIR` is where the trace data is collected as SQLite files and whatever else the tracer produces. Here a subdirectory is created and named after the current date and time. That subdirectory has the following structure:

```
+ data                                   # tracer output
|----o package1-vignette1.sqlite         # data for vignette1 from package1
|----o package1-vignette1.OK             # exists if executed to completion without error
|----o package1-vignette2.sqlite
|----o package1-vignette2.sqlite-journal # exists if there was an error while tracing
|----o package2-vignette1.sqlite
|----o package2-vignette1.OK
|----o ...
+ logs 
|----o package1.log                      # runtime logs, stdout and stderr (per package)
|----o ...
+ vignettes
|----o package1-vignette1.R              # instrumented vignette source code
|----o package1-vignette2.R
|----o package2-vignette1.R
|----o ...
```

The tracer sets the following environmental variables before tracing:
``` bash
export R_COMPILE_PKGS=1
export R_DISABLE_BYTECODE=0
export R_ENABLE_JIT=3
export R_KEEP_PKG_SOURCE=yes            
export RDT_COMPILE_VIGNETTE=false
```

These may only be changed by changing `dyntrace/packages.sh`

### Trace vignettes for specific packages

``` bash
make trace PROCESSES=1 DATA_DIR="$DATA_ROOT/R/traces/$HOSTNAME/" PACKAGES="stringr dplyr grid"
```

`PACKAGES` specifies the packages whose vignettes should be run. There is no more fine-grained execution unit than a package.

### Specifying a tracer

``` bash
make trace PROCESSES=1 DATA_DIR="$DATA_ROOT/R/traces/$HOSTNAME/" TRACER=../betterR-dyntrace/bin/R
```

The `TRACER` variable must point at an R executable.

## Checking data during/after tracing

Produces a report of which vignettes were processed and which traces were produced from correct execution. The results are printed to screen and saved in a `health.txt` file in the root of `DATA_DIR`

```
make check DATA_DIR="$DATA_ROOT/R/traces/$HOSTNAME/2017-12-15-01-42"
```

The report has the following format. There is a row for each executed vignette. The row consists of the following columns:

- package name
- vignette title
- size of sqlite file
- executed correctly (produced an OK file): TRUE/FALSE

Finally there's a newline and a summary line of the form: `SUCCEEDED:  n FAILED:  n  TOTAL:  n`

## Get list of packages that need to be re-run

Gets list of packages whose vignettes did not execute correctly. Stores the list in `$DATA_DIR/rerun.csv`

```
make get-todo-packages DATA_DIR="$DATA_ROOT/R/traces/$HOSTNAME/2017-12-15-01-42"

```

If the data dir was cleaned up this will no longer find any packages to re-run.

## Removing failed traces

Removes data from traces that did not produce an OK file form `DATA_DIR`.

```
make clean DATA_DIR="$DATA_ROOT/R/traces/$HOSTNAME/2017-12-15-01-42"
```

## Aggregating data for each vignette

Post-processes the traces and generates a CSV files with aggregated data for each trace. 

```
make csvs DATA_DIR="$DATA_ROOT/R/traces/$HOSTNAME/2017-12-15-01-42"
```

The data is stored in a `csv` directory inside `DATA_DIR`. The `csv` directory contains a subdirectory called `package-vignette` which contains the followins CSV files:

- `metadata.csv` - environmental variables and other metadata used by the tracer
- `basic_info.csv` - basic information about the gathered data: number of promises, number of calls, etc.
- `actual_distances.csv`   
- `call_strictness_by_type.csv`
- `call_strictness_rate.csv`
- `call_strictness_ratio.csv`
- `call_strictness.csv`
- `call_types.csv`
- `compiled_calls.csv`
- `compiled_functions.csv`
- `evaluation_order.csv`
- `forces_by_type.csv`
- `forces.csv`
- `function_strictness_by_type.csv`
- `function_strictness_rate.csv`
- `function_strictness.csv`
- `function_types.csv`
- `fuzzy_forces.csv`
- `promise_code_to_return_types.csv`
- `promise_evaluations.csv`
- `promise_full_types.csv`
- `promise_types.csv`
- `promises_forced_by_another_promise.csv`
- `promises_forcing_other_promises.csv`
- `return_types.csv`
- `specific_calls.csv`

This stage is execute in the OS's R by default. This means that OS's R has to have the following packages installed:

- optparse
- dplyr
- dbplyr
- RSQLite
- hashmap
- stringr

Here's the command for installing them:

``` r
install.packages(c("optparse", "dplyr", "dbplyr", "RSQLite", "hashmap", "stringr"))
```

## Aggregating data for all vignettes together

Aggregates the CSVs together to create a single collection of CSVs containing information from all traces. The aggregate will be created in a directory called `_all` in `DATA_DIR/csv`.

```
make aggregate-csvs DATA_DIR="$DATA_ROOT/R/traces/$HOSTNAME/2017-12-15-01-42"
```

## Making a huge CSV

Using aggregated files, creates a single mostly-human-readable summary CSV containing all the information (actually it skips two tables, because I don't know hot to turn them into columns).

```
make conglomerate-csvs DATA_DIR="$DATA_ROOT/R/traces/$HOSTNAME/2017-12-15-01-42"
```


## Generating a report

(WIP)

```r
make report DATA_DIR="$DATA_ROOT/R/traces/$HOSTNAME/2017-12-15-01-42"
```
