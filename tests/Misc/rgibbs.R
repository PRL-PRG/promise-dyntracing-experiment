library(promisedyntracer)

dyntrace_promises({
# ------------------------------------------------------------------
# Contributed by Michel Lang, TU Dortmund
# ------------------------------------------------------------------
# Code from https://github.com/fonnesbeck/useRshootout/blob/master/R/simplegibbs.R
# Obtain a sequence of random samples from multivariate probability distribution
# http://en.wikipedia.org/wiki/Gibbs_sampling
Rgibbs <- function(N,thin) {
    mat <- matrix(0,ncol=2,nrow=N)
    x <- 0
    y <- 0
    for (i in 1:N) {
        for (j in 1:thin) {
            x <- rgamma(1,3,y*y+4)
            y <- rnorm(1,1/(x+1),1/sqrt(2*(x+1)))
        }
        mat[i,] <- c(x,y)
    }
    mat
}

# change these numbers to get a suitable runtime
Rgibbs(1000, 100)
}
, 'rgibbs.sqlite'
, verbose=FALSE
, truncate=TRUE)



