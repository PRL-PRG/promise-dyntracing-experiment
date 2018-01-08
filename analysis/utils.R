typename <- function(type) {
    switch(toString(type),
           "0" = "NIL",
           "1" = "SYM",
           "2" = "LIST",
           "3" = "CLO",
           "4" = "ENV",
           "5" = "PROM",
           "6" = "LANG",
           "7" = "SPECIAL",
           "8" = "BUILTIN",
           "9" = "CHAR",
           "10" = "LGL",
           "13" = "INT",
           "14" = "REAL",
           "15" = "CPLX",
           "16" = "STR",
           "17" = "DOT",
           "18" = "ANY",
           "19" = "VEC",
           "20" = "EXPR",
           "21" = "BCODE",
           "22" = "EXTPTR",
           "23" = "WEAKREF",
           "24" = "RAW",
           "25" = "S4",
           "30" = "NEW",
           "31" = "FREE",
           "99" = "FUN",
           "?")
}

eventname <- function(event) {
    switch(toString(event),
           "0" = "CREATED",
           "1" = "LOOKED UP",
           "2" = "GARBAGE COLLECTED",
           "?")
}
