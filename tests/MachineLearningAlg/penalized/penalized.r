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

library(penalized)

n <- 200L # number of observations
p <- 100L # number of genes (typically around 22.000)

# random covariate matrix
x <- matrix(rnorm(n * p), n, p)
colnames(x) <- sprintf(paste("gene%0", nchar(p), "i", sep = ""), seq_len(p))


# model survival with influence of first 10 genes
lambda <- 10
beta <- c(rep(1, 10L), rep(0, p - 10L))
real.time <- -(log(runif(n))) / (lambda * exp(drop(x %*% beta)))
cens.time <- rexp(n, rate = 1/lambda)
status <- real.time <= cens.time
obs.time <- ifelse(status, real.time, cens.time)
surv <- Surv(obs.time, status)



prof <- profL1(response = surv, penalized = x, fold = 10L, trace = FALSE)
lambda.opt <- prof$lambda[which.max(prof$cvl)]
steplength <- abs(prof$lambda[2L] - prof$lambda[1L])
opt <- optL1(response = surv, penalized = x, fold = 10L,
             minlambda = lambda.opt - 2 * steplength,
             maxlambda = lambda.opt + 2 * steplength,
             trace = FALSE)

