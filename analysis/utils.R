library(hashmap)

update_argument_name <- function(name) {
  new_name <- if(str_detect(name, "\\.\\.\\..\\d+."))
                "..."
              else name
  new_name
}

memory_size_labels <-
  function(x, digits = 1) {
    ifelse(x == 0,
           "0 B",
           { y <- log2(x)
             units <- c("B", "KB", "MB", "GB", "TB", "PB")
             unit <- units[y %/% 10 + 1]
             value <- 2 ^ (y %% 10)
             paste(round(value, digits), unit, sep = " ") })
  }

count_labels <-
  Vectorize(function(x, digits = 2) {

    paste_round <- function(value, div, suffix, sep)
        paste(round(value/div, digits), suffix, sep = sep)

    if(is.na(x)) {
      "NA"
    } else if(x < 10^3) {
      paste0(x)
    } else if(x < 10^6) {
      paste_round(x, 1000, "K", sep=" ")
    } else if(x < 10^9) {
      paste_round(x, 10^6, "M", sep=" ")
    } else if(x < 10^12) {
      paste_round(x, 10^9, "B", sep=" ")
    } else {
        paste_round(x, 10^12, "T", sep=" ")
    }
  },
  "x")

relative_labels <-
  function(x) {
    percent_labels(x * 100)
  }

percent_labels <-
  function(x) {
    paste0(x, "%", sep=" ")
  }

extract_package_name <-
  function(name) {
    str_extract(name, "^.*::")
  }

is_value <-
  function(type) {
              ## SYM PROM LANG  DOT EXPR  FUN
    !(type %in% c(1,  5,   6,   17,  20,  99))
  }

to_named_values <-
  function(df, column_name) {

    underscore_to_camel_case <-
      function(name) {
        name %>%
          str_replace_all("_", " ") %>%
          str_to_title() %>%
          str_replace_all(" ", "")
      }

    named_values <- list()
    rownames(df) <- df[[column_name]]
    for(rowname in rownames(df)) {
      for(colname in colnames(df)) {
        if(colname != column_name) {
          var_name <-
            underscore_to_camel_case(paste(rowname, colname, collapse="_"))
          named_values[[var_name]] <- df[rowname, colname]
        }
      }
    }
    named_values
  }

classify_type <- function(type) {
  ifelse(
    type %in% c("character", "closure", "double", "environment", "integer", "list", "logical", "NULL"),
    "Value",
  ifelse(type %in% c("promise"),
         "Promise",
         "Expression"))
}


