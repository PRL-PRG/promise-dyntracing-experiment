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
           "69" = "OMEGA",
           "99" = "FUN",
           "NA" = "NA",
           "?")
}

eventname <- function(event) {
    switch(toString(event),
           "0" = "CREATED",
           "1" = "LOOKED UP",
           "2" = "GARBAGE COLLECTED",
           "?")
}

modename <- function(numeric_mode) {
  if(is.na(numeric_mode)) "UNACCOUNTED"
  else if (numeric_mode == 0) "CUSTOM"
  else "DEFAULT"
}

full_type_to_final_type <- function(full_type) {
  locs <- stri_locate_last(full_type, regex=",")
  locs[is.na(locs)] <- 0
  locs[, "start"] <- locs[, "start"] + 1
  locs[, "end"] <- -1
  str_sub(full_type, locs)
  #if(is.na(begin)) full_type
  #else str_sub(full_type, begin + 1, -1)
}

evaluation_modename <- function(strict, lazy) {
  if(strict == 0 && lazy == 0) "UNKNOWN"
  else if(lazy == 0) "STRICT"
  else if(strict == 0) "LAZY"
  else "MIXED"
}
