library(promisedyntracer)

dyntrace_promises({
# ------------------------------------------------------------------
# This material is distributed under the GNU General Public License
# Version 2. You may review the terms of this license at
# http://www.gnu.org/licenses/gpl-2.0.html
#
# Copyright (c) 2012-2016, Michel Lang, Helena Kotthaus,
# TU Dortmund University
#
# All rights reserved.
#
# Exemplary moving median on a time series
# Moving averages are used to smooth time series and reduce the noise.
# ------------------------------------------------------------------

# change these numbers to get a suitable runtime
n <- 1000   # number of observation in time series
width <- 11 # window width, must be odd

####################################################################################
### benchmark code below
####################################################################################

ts <- sqrt(1:n) + rnorm(n) # time series
hwidth <- width %/% 2
smoothed <- rep(NA, n)
for(i in seq(hwidth, n - hwidth)) {
  smoothed[i] <- median(ts[(i-hwidth):(i+hwidth)])
}

# check smoothing
if (FALSE) {
  plot(ts, type = "l")
  lines(smoothed, col = "red")
}
}
, 'moving_average.sqlite'
, verbose=FALSE
, truncate=TRUE)
