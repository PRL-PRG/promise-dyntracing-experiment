library(promisedyntracer)

dyntrace_promises({
  f <- function(a, b) { if(a) b else "abcd" }
  f(TRUE, 3)
  f(FALSE, 4)

  g <- function(c, d) { if(d) "efgh" else c }
  g(3, TRUE)
  g(4, FALSE)
}
, '/home/aviral/projects/promise-dyntracing-experiment/test/data/test.sqlite'
, verbose=FALSE)

