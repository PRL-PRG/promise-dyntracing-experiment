library(promisedyntracer)

dyntrace_promises({
# ------------------------------------------------------------------
# This material is distributed under the GNU General Public License
# Version 2. You may review the terms of this license at
# http://www.gnu.org/licenses/gpl-2.0.html
#
# Copyright (c) 2012-2016, Jakob Richter, Michel Lang, Helena Kotthaus,
# TU Dortmund University
#
# All rights reserved.
#
# Program for evaluating different parameter configurations of a
# SVM classification task with the mlr library in parallel.
# Input data can be downloaded from e.g.:
# http://www.csie.ntu.edu.tw/~cjlin/libsvmtools/datasets/binary/w1a
#
# ------------------------------------------------------------------
set.seed(1)

library(mlr)
library(gridExtra)

args        <- commandArgs(trailingOnly=TRUE)
data_file   <- args[1]
cpu_cores   <- as.integer(args[2])
sample_rows <- as.integer(args[3])

if (is.na(data_file)) data_file <- "w1a"
if (is.na(cpu_cores)) cpu_cores <- 2

##############################################
libsvm.read = function(file) {
  library(e1071)
  library(Matrix)
  dataset = read.matrix.csr(file)
  colNames = sapply( (1:(dim(dataset$x)[2])), FUN = function(x) { paste("X",x, sep = "") })
  dataframe = as.data.frame(as.matrix(dataset$x))
  colnames(dataframe) = colNames
  dataframe$Y = dataset$y
  dataframe
}

# http://www.csie.ntu.edu.tw/~cjlin/libsvmtools/datasets/binary/w1a
data = libsvm.read(data_file)
task = makeClassifTask(data = data, target = "Y")

library(parallelMap)
parallelStartMulticore(cpu_cores)

lrn = makeLearner("classif.LiblineaRMultiClass")
par.set = makeParamSet(
  makeNumericParam("cost", lower = -15, upper = 15, trafo = function(x) 2^x),
  makeNumericParam("epsilon", lower = -15, upper = 15, trafo = function(x) 2^x))
rsi = makeResampleDesc(method = "Holdout", stratify = TRUE)
tune.ctrl = makeTuneControlGrid(resolution = 10)

t.res = tuneParams(learner = lrn, task = task, resampling = rsi, 
        measures = list(mmce, timetrain, timepredict, timeboth), 
        par.set = par.set, control = tune.ctrl)

op.df = as.data.frame(t.res$opt.path)

parallelStop()
}
, 'LibSVMMulticore.sqlite'
, verbose=FALSE
, truncate=TRUE)
