#------------------------------------------------------------------
# This material is distributed under the GNU General Public License
# Version 2. You may review the terms of this license at
# http://www.gnu.org/licenses/gpl-2.0.html
#
# Copyright (c) 2012-2016, Michel Lang, Helena Kotthaus,
# TU Dortmund University
#
# All rights reserved.
#
# Installs the R packages needed for the machine learning
# benchmarks.
#
# USEAGE: Rscript ./MLPkgIns.R 
# ------------------------------------------------------------------
pkgs <- c("CoxBoost", "mlr", "BBmisc", "reshape", "abind", "boot",
"codetools", "ROCR",
          "ParamHelpers", "ada", "adabag", "DiceKriging", "e1071", "earth",
          "FNN", "gbm", "kernlab", "kknn", "klaR", "mboost", "mda", "nnet",
          "party", "penalized", "pls", "randomForest", "rpart", "rsm",
"RWeka", "testthat")

cat("Installing the following packages:\n")
print(pkgs)
install.packages(pkgs, dep = TRUE, repos="http://cran.at.r-project.org")
