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

evaluation_modename <- function(always_forced, never_forced) {
  if(always_forced == 0 && never_forced == 0) "UNKNOWN"
  else if(never_forced == 0) "ALWAYS FORCED"
  else if(always_forced == 0) "NEVER FORCED"
  else "SOMETIMES FORCED"
}


function_evaluation_modename <- function(argument_evaluation_modename) {
  if("SOMETIMES FORCED" %in% argument_evaluation_modename) "SOMETIMES"
  else if("NEVER FORCED" %in% argument_evaluation_modename) "PARTIAL"
  else "STRICT"
}

purityname <- function(purity) {
  if(purity) "PURE" else "SIDE-EFFECTING"
}


update_argument_name <- function(name) {
  new_name <- if(str_detect(name, "\\.\\.\\..\\d+."))
                "..."
              else name
  new_name
}

memory_size_labels <-
  function(x) {
    y = log2(x)
    units <- c("B", "KB", "MB", "GB", "TB", "PB")
    unit <- units[y %/% 10 + 1]
    value <- 2 ^ (y %% 10)
    paste(value, unit, sep = " ")
  }

count_labels <-
  Vectorize(function(x) {
    if(is.na(x)) {
      "NA"
    } else if(x < 10^3) {
      paste0(x)
    } else if(x < 10^6) {
      paste(x/1000, "THOUSAND", sep=" ")
    } else if(x < 10^9) {
      paste(x/(10^6), "MILLION", sep=" ")
    } else {
      paste(x/(10^9), "BILLION", sep=" ")
    }
  },
  "x")

relative_labels <-
  function(x) {
    paste0(x * 100, "%", sep=" ")
  }

extract_package_name <-
  function(name) {
    str_extract(name, "^.*::")
  }

pp <- function(number) format(number, big.mark=",", scientific=FALSE, trim=FALSE, digits=2)
pp_trunc <- function(x) ifelse(x==0, paste(format(x, digits=2, scientific=FALSE)),
                               ifelse(x < 1000, format(x, digits=2, scientific=FALSE), 
                                      ifelse(x < 1000000, 
                                             paste(format(floor((x/1000)*10)/10, digits=2, scientific=FALSE), "k", sep=""),
                                             ifelse(x < 1000000000,
                                                    paste(format(floor((x/1000000)*10)/10, digits=2, scientific=FALSE), "m", sep=""),
                                                    paste(format(floor((x/1000000000)*10)/10, digits=2, scientific=FALSE), "b", sep="")))))
pp_perc <- function(x) dollar_format(prefix="", suffix="%")(x)
