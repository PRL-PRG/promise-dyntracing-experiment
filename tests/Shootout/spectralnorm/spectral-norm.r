library(promisedyntracer)

dyntrace_promises({
#!/usr/bin/env Rscript
# ------------------------------------------------------------------
# The Computer Language Shootout
# http://shootout.alioth.debian.org/
#
# This material is distributed under the GNU General Public License
# Version 2. You may review the terms of this license at
# http://www.gnu.org/licenses/gpl-2.0.html
#
# Copyright (c) 2012-2016, Michel Lang, Helena Kotthaus,
# TU Dortmund University
#
# All rights reserved.
# ------------------------------------------------------------------

n <- 20 #as.integer(commandArgs(trailingOnly = TRUE)[1])

u <- rep(1.0, n)
j <- seq(from = 0L, to = n - 1L)
i <- rep(seq_len(n), each = n)

M <- 1.0 / matrix((i + j) * (i + j - 1L) / 2L + i, nrow = n, ncol = n, byrow = TRUE)
Mt <- t(M)

for(i in seq_len(10L)) {
    v <- u %*% Mt %*% M
    u <- v %*% Mt %*% M
}

cat(sprintf("%0.9f\n", sqrt(sum(u %*% t(v)) / sum(v %*% t(v)))))
}
, 'spectral-norm.sqlite'
, verbose=FALSE
, truncate=TRUE)
