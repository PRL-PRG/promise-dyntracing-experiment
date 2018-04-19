# mlr benchmarks #

This directory contains data files and R scripts for running multiple machine learning algorithms on various data sets using the mlr package.

## Installation ##

To install the mlr package and all packages it needs, just run
    Rscript MLPkgIns.R

It should automatically download all R packages you need to run the benchmarks in this directory. To verify that the installation was successful, you can use `Rscript -e 'mlr::listLearners("classif")'` to get a list of available classification learners.

If you want to use our automated benchmark driver `autobench.pl` which measures the time for each R script and stores the results in an SQLite database, you need to edit the `autobench.conf` file in this directory and modify at least the `INTERPRETER_DIR` variable at the top with the correct path to your installed R interpreter. If you want to use additional interpreters for comparison, you can add blocks for them; the syntax of the file is described in its comments.

## Manual Usage ##

There are two R scripts for the machine learning benchmarks in this directory, `bench-sim.R` and `bench-data.R`. The first one uses simulated data sets, the second one real datasets gathered from the UCI repository. Both use the same command line arguments:

    R -f bench-sim.R  --args [type] [task] [learner] [repl]
    R -f bench-data.R --args [type] [task] [learner] [repl]

* _type_: either "regr" for regression or "classif" for classification.
      For mlrdata benchmarks: (for now) only classif supported
* _task_: file in directory "simulated" or "mlrdata-cache" (w/o preceding "[type]_")
     which identifies the task
* _learner_: a mlr learner identifier, see below
* _repl_: number of replications, e.g. subsample repetitions

You can see the list of available learners for regression by using `Rscript -e 'mlr::listLearners("regr")'`and the list of learners for classification by using `Rscript -e 'mlr::listLearners("classif")'`.

### Examples ###

    R -f bench-sim.R  --args regr sim01 lm 100
    R -f bench-sim.R  --args classif sim10 randomForest 10
    R -f bench-data.R --args classif Vowel rpart 10

## Automation ##

To use `autobench.pl`, you need a Perl interpreter with version 5.10 or higher and at least the DBI and DBD::SQLite modules which can usually be installed using your operating system's package management if they aren't already installed.

`autobench.pl` reads the `autobench.conf` file in either the current or its own directory, where multiple R interpreters can be defined to run scripts with. The interpreter(s) to use are selected using one or more `--interpreter NAME` arguments. The argument `--repetitions NUM` sets the number of times each R program should be run in each interpreter, e.g. `-repetitions 3 --interpreter r31 --interpreter r31nojit` would run each script three times with the "r31" and "r31nojit" interpreters. All other command line arguments are assumed to be R programs to run, if they need any additional command line arguments like the two benchmark scripts in this directory, the name of the R program plus its arguments need to be given in quotes.

`autobench.pl` stores the time needed to run each R script in an SQLite database named `output.sqlite`. The format of the tables should be self-explanatory.

An example shell script that uses `autobench.pl` to run a series of machine learning benchmarks using one of the UCI data sets can be found in this directory as `runDemo.sh`.

    ./runDemo.sh

`autobench.pl` has a few features to add additional measured data into the database which are currently not documented.

