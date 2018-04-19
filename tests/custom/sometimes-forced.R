library(promisedyntracer)

dyntrace_promises({

e <- function() 2
f <- function() 1

f()        # f strict
#f(e())     # ? allowed - no

g <- function(x) 25

g(e())
g(2)       # never strict


h <- function(x)  x

h(e())
h(2)
h(x = 2)     # h strict


i <- function(x = 1) 1

i()          # ?


j <- function(x = 23) x

j()         # j strict


k <- function(x = 1) x

k(x = e())   # k strict

sometimes <- function(x, y) if(x) y else 23 # sometimes
sometimes(TRUE, 90)
sometimes(FALSE, 91)

sometimes_2 <- function(x = TRUE, y) if(x) y else 24 # sometimes
sometimes_2(y = 90)
sometimes_2(FALSE, 91)

sometimes_3 <- function(x, y) if(x) y else 25 # sometimes
sometimes_3(y = 90, x = TRUE)
sometimes_3(x = FALSE, y = 91)

sometimes_4 <- function(x, ...) if(x) list(...) else 26 # sometimes
sometimes_4(TRUE, 2, 3, 4)
sometimes_4(TRUE, 2, 3)
sometimes_4(FALSE, 2, 3)

sometimes_5 <- function(x, ...) ..3 # sometimes
sometimes_5(TRUE, 2, 3, 4)

sometimes_6 <- function(x, ...) if(x) ..2 else ..1 # sometimes
sometimes_6(TRUE, 2, 3)
sometimes_6(FALSE, 2, 3)

sometimes_caller_1 <- function(x, y) x + sometimes(x, y) # sometimes
sometimes_caller_1(TRUE, 90)
sometimes_caller_1(FALSE, 91)

sometimes_caller_2 <- function(x, y) sometimes(x, y) + sometimes(!x, y)  # this is strict
sometimes_caller_2(TRUE, 90)
sometimes_caller_2(FALSE, 91)

sometimes_caller_3 <- function(...) sometimes_4(...)
sometimes_caller_3(TRUE, 3, list(4.6))
sometimes_caller_3(FALSE, 3, 4)

sometimes_caller_4 <- function(x, y, z) {  #strict
  sometimes_caller_5 <- function(a, b) sometimes_2(a, b) #sometimes

  sometimes_caller_5(x, y) + sometimes_caller_5(!x, y) +
    sometimes_caller_5(x, z) + sometimes_caller_5(!x, z)
}
sometimes_caller_4(TRUE, 3, 4.6)

sometimes_caller_6 <- function(x, y, z) {  #lazy
  sometimes_caller_7 <- function(c, d) sometimes_2(c, d) #sometimes

  sometimes_caller_7(x, y) + sometimes_caller_7(!x, y)
}
sometimes_caller_6(TRUE, 3, 4.6)

}
, 'test.sqlite'
, verbose=FALSE)

