## This test case results in creation of nested promises and
## the promises are pre evaluated as their value is known ahead
## of time. This is done within the C code.
## Argument 'x' of the function being applied is bound to
## a promise that contains an expression that evaluates to
## the current value being applied to.
lapply(c(1, 2, 3), function(x) print(x))
