# ------------------------------------------------------------------
# This material is distributed under the GNU General Public License
# Version 2. You may review the terms of this license at
# http://www.gnu.org/licenses/gpl-2.0.html
#
# Copyright (c) 2012-2016, Michel Lang, Helena Kotthaus,
# TU Dortmund University
#
# All rights reserved.
# ------------------------------------------------------------------

library(BBmisc)
library(mlr)

args = as.list(commandArgs(TRUE))
stopifnot(length(args) == 4L)
names(args) = c("type", "task", "learner", "repls")
args$repls = as.integer(args$repls)

# FIXME we move this to openml soon
task = load2(file.path("mlrdata-cache", sprintf("%s_%s.RData", args$type, args$task)))
learner = makeLearner(sprintf("%s.%s", args$type, args$learner))
rdesc = makeResampleDesc("Subsample", iters = args$repls)
resampled = resample(learner, task, rdesc)
print(resampled$aggr[1L])
