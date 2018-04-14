library(promisedyntracer)

dyntrace_promises({

  f1 <- function(a = {x}) {
    x <- 1
    a
  }

  f1()

  f2 <- function(a = {y}, b = {y <- 1}) {
     b
     a
  }

  f2()

  f3 <- function(a = { c },
                 b = { c <- 34; a }) {
    b
  }

  f3()

  f4_global <- 100
  f4 <- function(a = {f4_global}) {
    f4_global <- 1
    a
  }

  f4()

  g5 <- function(a, b) {
    b
    a
  }

  f5 <- function() {
    g5(x + y, {x <- 234; y <<- 64})
  }

  f5()
}
, 'interference.sqlite'
, verbose=FALSE
, truncate=TRUE)

