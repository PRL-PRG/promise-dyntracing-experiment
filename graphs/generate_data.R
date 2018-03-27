suppressPackageStartupMessages(library("dplyr"))
suppressPackageStartupMessages(library("stringr"))
suppressPackageStartupMessages(library("hashmap"))

pp <- function(number) 
  format(number, big.mark=",", scientific=FALSE, trim=FALSE, digits=2)

log_line_to_csv <- function(where, ...) {
  what <- data.frame(...)
  header <- !file.exists(where)
  write.table(what, file=where, sep=",", append=TRUE, col.names=header, row.names=FALSE)
}

store <- function(name, output_path) function(table) function(...) {
  path <- file.path(output_path, paste(table, "csv", sep="."))
  write(paste("writing", path), stderr())
  suppressWarnings(log_line_to_csv(path, name=name, ...))
}

load <- function(input_path) function (csv_file) {
    path <- file.path(input_path, paste0(csv_file, ".csv"))
    write(paste("reading", path), stderr())
    as_tibble(read.csv(path, stringsAsFactors=FALSE))
}

humanize_full_promise_type = function(full_type) {
  strsplit(full_type, ',', fixed=TRUE) %>% 
    sapply(., as.numeric) %>% 
    sapply(., function(type) humanize_promise_type(type)) %>% 
    #lapply(., function(vec) paste(vec, collapse = "→"))
    paste(., collapse = "→")
}

make_labels = function(x) ifelse(is.na(x), "NA", x)
guard_against_na <- function(key, map) ifelse(is.na(key), "NA", map[[key]])

FUNCTION_TYPE_NAMES <- c("closure", "built-in", "special", "true built-in")
FUNCTION_TYPE_CODES <- 0:3
FUNCTION_TYPES <- hashmap(keys=FUNCTION_TYPE_CODES, values=FUNCTION_TYPE_NAMES)
FUNCTION_TYPES_REV <- hashmap(keys=FUNCTION_TYPE_NAMES, values=FUNCTION_TYPE_CODES)

humanize_function_type <- function(type) guard_against_na(type, FUNCTION_TYPES)
dehumanize_function_type <- function(type) guard_against_na(type, FUNCTION_TYPES_REV)

SEXP_TYPE_NAMES <- c(
    "NIL", "SYM", "LIST", "CLOS", "ENV",  "PROM", # 0-5
    "LANG", "SPECIAL", "BUILTIN", "CHAR",  "LGL", # 6-10
    "INT", "REAL", "CPLX", "STR", "DOT", "ANY",   # 13-18
    "VEC", "EXPR", "BCODE", "EXTPTR", "WEAKREF",  # 19-23
    "RAW", "S4",                                  # 24-25
    "actbind", "...")                             # 42, 69 
SEXP_TYPE_CODES <- c(0:10,13:25,42,69) 
SEXP_TYPES <- hashmap(keys=SEXP_TYPE_CODES, values=SEXP_TYPE_NAMES)
SEXP_TYPES_REV <- hashmap(keys=SEXP_TYPE_NAMES, values=SEXP_TYPE_CODES)

humanize_promise_type <- function(type) guard_against_na(type, SEXP_TYPES) 
dehumanize_promise_type <- function(type) guard_against_na(type, SEXP_TYPES_REV)

get_forces <- function(promises, promise.forces, n.promises, cutoff=NA) {
  data <- promises %>% rename(promise_id=id) %>% 
    left_join(promise.forces, by="promise_id") %>% 
    select(promise_id, event_type) %>% collect
  
  unevaluated <- tibble(
    no.of.forces=0, 
    number=(data %>% filter(is.na(event_type)) %>% count %>% data.frame)$n
  )
  evaluated <- 
    data %>% filter(!is.na(event_type)) %>% 
    group_by(promise_id) %>% count %>% 
    group_by(n) %>% count %>% ungroup %>% 
    rename(no.of.forces=n, number=nn) 
  new.data <- rbind(unevaluated, evaluated)
  
  if (is.na(cutoff)) {
    new.data %>% mutate(percent=((number*100/n.promises)))
  } else {
    above <- 
      new.data %>% filter(no.of.forces > cutoff) %>% 
      ungroup %>% collect %>% 
      summarise(no.of.forces=Inf, number=sum(number))
    below <- new.data %>% filter(no.of.forces <= cutoff)
    rbind(above, below) %>% mutate(percent=((number*100/n.promises)))
  }
}

get_fuzzy_forces <- function(promises, promise_evaluations, n.promises) {
  renamer = hashmap(0:4, 
                    c("accessed 2+ times, forced 2+ times", 
                      "accessed 2+ times, forced once", 
                      "accessed once, forced once", 
                      "accessed 1+ times, never forced", 
                      "never accessed"))
  promises %>% rename(promise_id = id) %>% 
  left_join(promise_evaluations, by ="promise_id") %>% 
  group_by(promise_id) %>% 
  summarise(
    forced = ifelse(is.na(event_type), 0, sum(as.integer(event_type == 15))), 
    looked_up = ifelse(is.na(event_type), 0, sum(as.integer(event_type == 0)))) %>% 
  mutate(classification =
              ifelse((forced > 1),                    0,               # evaluated 2+ times, forced 2+ times
              ifelse((forced == 1 && looked_up > 0),  1,               # evaluated 2+ times, forced once
              ifelse((forced == 1 && looked_up ==0 ), 2,               # evaluated once, forced once
              ifelse((forced == 0 && looked_up > 0),  3,               # evaluated 1+ times, never forced
              ifelse((forced == 0 && looked_up == 0), 4, NA)))))) %>%  # never evaluated
  group_by(classification) %>% 
  summarise(number=n()) %>% as.data.frame %>%
  mutate(percent=(100*number/n.promises)) %>% 
  right_join(data.frame(classification=0:4), by="classification") %>%
  mutate(
    number=ifelse(is.na(number), 0, number),
    percent=ifelse(is.na(percent), 0, percent),
    classification = ifelse(is.na(classification), NA, renamer[[classification]]))
}

get_promise_evaluations <- function(promises, promise_evaluations, n.promises, cutoff=NA) {
  data <- 
    promises %>% rename(promise_id = id) %>% 
    left_join(promise_evaluations, by="promise_id") %>% 
    collect
  
  unevaluated <- tibble(
    no.of.evaluations=0, 
    number=(data %>% filter(is.na(event_type)) %>% count %>% data.frame)$n)
  
  evaluated <- 
    data %>% filter(!is.na(event_type)) %>% 
    group_by(promise_id) %>% count %>% 
    group_by(n) %>% count %>% 
    ungroup %>% rename(no.of.evaluations=n, number=nn)
  
  new.data <- rbind(unevaluated, evaluated) 
  
  {
    if (is.na(cutoff)) {
      new.data %>% mutate(percent=(number*100/n.promises))
    } else {
      above <- new.data %>% filter(no.of.evaluations > cutoff) %>% 
        ungroup %>% collect %>% 
        summarise(no.of.evaluations=Inf, number=sum(number))
      below <- new.data %>% filter(no.of.evaluations <= cutoff)
      rbind(below, above) %>% mutate(percent=(number*100/n.promises))
    } 
  }%>% 
    mutate(no.of.evaluations=
           ifelse(no.of.evaluations==Inf,
                  paste(">",cutoff, sep=""),
                  as.character(no.of.evaluations)))
}

get_promise_types <- function(promises, n.promises, cutoff=NA) {
  result <- 
    promises %>% 
    group_by(type) %>% count(type) %>% 
    arrange(type) %>%
    mutate(percent=((n*100/n.promises))) %>%
    group_by(type) %>% 
    do(mutate(., 
              type_code = type, 
              type = humanize_promise_type(type)
    )) %>%
    rename(number=n) %>%
    group_by(type) %>% select(type, number, percent) %>%
    data.frame %>%   
    arrange(desc(number))
  
  if (is.na(cutoff)) {
    result
  } else {
    above <- result %>% filter(percent >= cutoff)
    below <- result %>% filter(percent < cutoff) %>% 
      summarise(type="other", number=sum(number), percent=sum(percent))  
    rbind(above, below)
  }
}

get_full_promise_types <- function(promises, n.promises, cutoff=NA) {
  result <-
    promises %>% 
    group_by(full_type) %>% count(full_type) %>% 
    arrange(full_type) %>%
    mutate(percent=((n*100/n.promises))) %>%
    group_by(full_type) %>% 
    do(mutate(., 
              type = humanize_full_promise_type(full_type)
    )) %>%
    rename(number=n) %>%
    group_by(type) %>% select(type, number, percent) %>%
    data.frame %>%   
    arrange(desc(number))
  
  if (is.na(cutoff)) {
    result
  } else {
    above <- result %>% filter(percent >= cutoff)
    below <- result %>% filter(percent < cutoff) %>% 
      summarise(type="other", number=sum(number), percent=sum(percent))  
    rbind(above, below)
  }
}

get_promise_return_types <- function(promises, promise_returns, n.promises, cutoff=NA) {
  result <- 
    promises %>% rename(promise_id=id) %>% 
    select(promise_id) %>% left_join(promise_returns, by="promise_id") %>%
    group_by(type) %>% count(type) %>% 
    arrange(type) %>%
    mutate(percent=((n*100/n.promises))) %>%
    group_by(type) %>% 
    do(mutate(., 
              type_code = type, 
              type = humanize_promise_type(type)
    )) %>%
    rename(number=n) %>%
    group_by(type) %>% select(type, number, percent) %>%
    data.frame %>%   
    arrange(desc(number))
  
  if (is.na(cutoff)) {
    result
  } else {
    above <- result %>% filter(percent >= cutoff)
    below <- result %>% filter(percent < cutoff) %>% 
      summarise(type="other", number=sum(number), percent=sum(percent))  
    rbind(above, below)
  }
}

get_promise_types_to_return_types <- function(promises, promise_returns, n.promises, cutoff=NA) {
  result <- 
    promises %>% rename(promise_id=id) %>% 
    select(promise_id, type) %>% 
    left_join(promise_returns %>% rename(return_type = type), by="promise_id") %>%
    do(mutate(., 
              type_resolution = paste(humanize_promise_type(type), 
                                      humanize_promise_type(return_type), sep="→"))) %>%
    group_by(type_resolution) %>% count(type_resolution) %>% 
    mutate(percent=((n*100/n.promises))) %>%
    rename(number=n, types=type_resolution) %>%
    select(types, number, percent) %>%
    data.frame %>%   
    arrange(desc(number))
  
  if (is.na(cutoff)) {
    result
  } else {
    above <- result %>% filter(percent >= cutoff)
    below <- result %>% filter(percent < cutoff) %>% 
      summarise(types="other", number=sum(number), percent=sum(percent))  
    rbind(above, below)
  }
}

get_forces_by_type <- function(promises, promise.forces, n.promises, n.promise.forces) {
  promise_types <- get_promise_types(promises, n.promises)
  promise_type_count <- hashmap(
    keys=promise_types$type,
    values=promise_types$number
  )
  
  data <- promises %>% rename(promise_id = id) %>% 
    left_join(promise.forces, by="promise_id") %>% 
    select(promise_id, type, full_type, event_type) %>% 
    collect
  
  unevaluated <- data %>% 
    filter(is.na(event_type)) %>% 
    group_by(type) %>% summarise(no.of.forces=as.integer(0), number=n()) %>% 
    as.data.frame
  
  evaluated <- data %>% 
    filter(!is.na(event_type)) %>% 
    group_by(promise_id) %>% summarise(no.of.forces=n(), type=c(type)) %>% 
    group_by(type, no.of.forces) %>% summarise(number=n()) %>% 
    as.data.frame
  
  intermediate <- 
    rbind(unevaluated, evaluated)
  
  histogram <- 
    merge( # cartesian product
      data.frame(type=intermediate$type %>% unique), 
      data.frame(no.of.forces=intermediate$no.of.forces %>% unique), 
      by=NULL) %>% 
    left_join(intermediate, by=c("type", "no.of.forces")) %>%
    mutate(number=ifelse(is.na(number), 0, number)) %>%
    arrange(type, no.of.forces) %>%
    mutate(type=humanize_promise_type(type)) %>%
    mutate(percent_within_type=((number*100/promise_type_count[[type]]))) %>%
    mutate(percent_overall=((number*100/n.promise.forces)))
  
  histogram
}

get_actual_distances <- function(promises, promise.forces, n.promise.forces, cutoff=NA) {
  actual.distances <-
    left_join(promises, promise.forces, by=c("id" = "promise_id"))  %>%
    select(id, from_call_id, in_call_id, lifestyle, actual_distance_from_origin) %>%
    
    group_by(actual_distance_from_origin) %>% count %>%
    mutate(percent=((n*100/n.promise.forces))) %>% rename(number = n)
  
  na.distance <- NA
  min.distance <- -1
  max.distance <- 
    (actual.distances %>% 
       filter(!is.na(actual_distance_from_origin)) %>% data.frame)$actual_distance_from_origin %>% 
    max
  distance.range <- c(na.distance, min.distance:max.distance)
  
  if (!is.na(cutoff)) {
    if (max.distance > cutoff) 
      max.distance <- cutoff
    
    below <- 
      actual.distances %>% 
      filter(is.na(actual_distance_from_origin) || actual_distance_from_origin <= cutoff)  %>%
      collect %>% ungroup %>%
      mutate(actual_distance_from_origin = actual_distance_from_origin)
    
    above <- 
      actual.distances %>% 
      filter(!is.na(actual_distance_from_origin)) %>% 
      filter(actual_distance_from_origin > cutoff) %>% 
      collect %>% ungroup %>% 
      summarise(
        actual_distance_from_origin=Inf, 
        number=sum(number), 
        percent=sum(percent))
    
    distance.range <- c(na.distance, min.distance:max.distance, Inf)
    
    actual.distances <- rbind(below, above)
  }
  
  histogram <- 
    data.frame(actual_distance_from_origin = distance.range) %>% 
    left_join(actual.distances, by="actual_distance_from_origin", copy=TRUE) %>% 
    rename(actual_distance = actual_distance_from_origin)
  
  histogram %>% 
    mutate(actual_distance=ifelse(actual_distance==Inf, 
                                  paste(">", cutoff, sep=""), 
                                  as.character(actual_distance)))
}

get_calls_by_type <- function(calls, functions, n.calls) {
  left_join(calls, select(functions, function_id, type), by="function_id") %>% 
    group_by(type) %>% count %>% rename(number=n) %>% 
    collect %>% ungroup() %>%
    mutate(type=humanize_function_type(type), percent=100*number/n.calls)
}

# TODO pass complete function table and n.functions
get_functions_by_type <- function(functions, n.functions) {
  functions %>% 
  group_by(type) %>% count %>% rename(number=n) %>% 
  collect %>% ungroup() %>%
  mutate(type=humanize_function_type(type), percent=100*number/n.functions)
}

get_call_compilations_by_type <- function(calls, functions, n.calls, specific_type=NA) {
  calls_by_type <- get_calls_by_type(calls, functions, n.calls)
  calls_by_type_hashmap <- hashmap(calls_by_type$type, calls_by_type$number)
  
  data <- 
    if (is.na(specific_type)) {
      left_join(calls, select(functions, function_id, type), by="function_id")
    } else {
      dehumanized_type <- dehumanize_function_type(specific_type)
      left_join(calls, select(functions, function_id, type), by="function_id") %>% 
        filter(type == dehumanized_type)
    }
  
  histogram <- data %>%
    group_by(type, compiled) %>% count %>% rename(number=n) %>% 
    collect %>% ungroup() %>%
    mutate(type=humanize_function_type(type)) %>% 
    mutate(compiled=ifelse(as.logical(compiled), "compiled", "uncompiled")) %>% 
    mutate(percent_overall=100*number/n.calls) %>%
    mutate(percent_within_type=100*number/calls_by_type_hashmap[[type]])
  
  { 
    if (is.na(specific_type))
      histogram
    else
      histogram %>% select(-type, -percent_overall)
  } %>% 
    rename(percent = percent_within_type)
}

get_function_compilations_by_type_aggregate_over_functions <- function (histogram, functions, n.functions, specific_type=NA) {
  functions_by_type <- get_functions_by_type(functions, n.functions)
  functions_by_type_hashmap <- hashmap(functions_by_type$type, functions_by_type$number)
  
  histogram <-  
    histogram %>%
    collect %>%
    group_by(function_id) %>% summarise(runs=sum(runs), compiled_runs=sum(compiled_runs), type=unique(type)) %>% 
    mutate(compiled=ifelse(compiled_runs == 0, 0, as.character(ifelse(runs == compiled_runs, 1, ifelse(compiled_runs == runs - 1, 2, 3))))) %>%
    group_by(type, compiled) %>% count %>% rename(number=n) %>%
    collect %>% ungroup() %>%
    mutate(compiled=ifelse(compiled == 1, "compiled", ifelse(compiled == 2, "after 1st", ifelse(compiled == 0, "uncompiled", "erratic")))) %>%
    mutate(type=humanize_function_type(type)) %>%
    mutate(percent_overall=100*number/n.functions) %>%
    mutate(percent_within_type=100*number/functions_by_type_hashmap[[type]])
    
  { if (is.na(specific_type))
    histogram
  else
    histogram %>% select(-type, -percent_overall) } %>% 
    rename(percent = percent_within_type)
}

# This one checks in the calls rather than in the function, so functions which get compiled on the fly will register as such.
get_function_compilations_by_type_actual <- function(calls, functions, n.functions, specific_type=NA, dont_aggregate_over_function_ids=FALSE) {
  specific_functions <- 
    if (is.na(specific_type)) {
      select(functions, function_id, type) 
    } else {
      dehumanized_type <- dehumanize_function_type(specific_type)
      select(functions, function_id, type) %>% filter(type == dehumanized_type)
    }
  
  histogram <- 
    left_join(specific_functions, select(calls, function_id, compiled), by="function_id") %>% 
    group_by(function_id) %>% summarise(runs=n(), compiled_runs=sum(compiled), type=type) 
  
  if (dont_aggregate_over_function_ids)
    return(histogram)
  else     
    return(get_function_compilations_by_type_aggregate_over_functions(histogram, functions, n.functions, specific_type))
}

# assumes one promise ==> one or zero forces
get_promises_forced_by_another_evaluations <- function(promises, promise.forces, n.promises) {
  promises %>% 
    rename(promise_id=id) %>% rename(created_in=in_prom_id) %>% select(promise_id, created_in) %>% 
    left_join(promise.forces, by="promise_id") %>% 
    rename(forced_in=in_prom_id) %>% select(promise_id, created_in, forced_in) %>% 
    mutate(forced_by_another=created_in!=forced_in) %>% 
    group_by(forced_by_another) %>% summarise(number=n()) %>% ungroup %>% 
    collect %>% 
    mutate(forced_by_another=as.logical(forced_by_another), percent=(100*number/n.promises))
}

get_cascading_promises <- function (promises, promise.forces, cutoff=NA) {
  basic <- promises %>% 
    rename(promise_id=id) %>% rename(created_in=in_prom_id) %>% select(promise_id, created_in) %>% 
    left_join(promise.forces, by="promise_id") %>% 
    rename(forced_in=in_prom_id) %>% 
    mutate(forced_by_another=created_in!=forced_in)
    
  nas <- 
    basic %>%
    filter(is.na(forced_by_another)) %>%
    count %>%
    rename (number=n) %>%
    mutate(number_of_forced_promises=NA) %>%
    select (number_of_forced_promises, number) %>%
    data.frame
  
  zero <- 
    basic %>% 
    filter(!is.na(forced_by_another)) %>%
    filter(!forced_by_another)%>%
    count %>%
    rename (number=n) %>%
    mutate(number_of_forced_promises=0) %>%
    select (number_of_forced_promises, number) %>%
    data.frame
  
  forcing <-
    basic %>%
    filter(!is.na(forced_by_another)) %>%
    filter(forced_by_another)%>%
    group_by(forced_in) %>% count %>% rename(number_of_forced_promises=n) %>% 
    group_by(number_of_forced_promises) %>% count %>% rename(number=n) %>%
    data.frame
  
  histogram <- rbind(nas, zero, forcing) %>% mutate(percent=100*number/sum(number))
  
  if (is.na(cutoff)) {
     histogram
  } else {
    above <- histogram %>% filter(number_of_forced_promises > cutoff) %>% ungroup %>% 
      summarise(number_of_forced_promises=Inf, number=sum(number), percent=sum(percent))
    below <- histogram %>%  filter(number_of_forced_promises <= cutoff)
    rbind(below, above)
  }
}

get_call_strictness <- function(calls, promise_associations, promise.forces, n.calls) {
  histogram <- 
    calls %>% left_join(promise_associations, by="call_id") %>% 
    filter(!is.na(promise_id)) %>%
    left_join(promise.forces, by="promise_id") %>% 
    group_by(call_id) %>% summarise(
      unevaluated=sum(as.integer(is.na(event_type))), 
      escaped=sum(as.integer(!is.na(event_type) && (lifestyle == 3))), 
      evaluated=sum(as.integer(!is.na(event_type) && (lifestyle != 3))), 
      count=n()) %>%
    collect %>% ungroup() %>%
    mutate(strict=(evaluated==count)) %>%
    group_by(strict) %>% 
    summarise(number=n(), percent=100*n()/n.calls)
  
  nas <-
    calls %>% left_join(promise_associations, by="call_id") %>% 
    filter(is.na(promise_id)) %>% 
    count() %>% rename(number=n) %>%
    mutate(strict=NA, percent=100*number/n.calls) %>%
    collect
  
  rbind(histogram, nas)
}

get_call_strictness_ratios <- function(functions, calls, promise_associations, promise.forces, n.calls) {
  histogram <- 
    calls %>% 
      left_join(functions, by="function_id")%>% 
      filter(type==0) %>%
      left_join(promise_associations, by="call_id") %>% 
      filter(!is.na(promise_id)) %>%
      left_join(promise.forces, by="promise_id") %>% 
      group_by(call_id) %>% summarise(
        unevaluated=sum(as.integer(is.na(event_type))), 
        escaped=sum(as.integer(!is.na(event_type) && (lifestyle == 3))), 
        evaluated=sum(as.integer(!is.na(event_type) && (lifestyle != 3))), 
        count=n()) %>%
      collect %>% ungroup %>%
      mutate(strictness_ratio=paste(evaluated, count, sep="/")) %>%
      rename(promises=count) %>%
      group_by(strictness_ratio, promises, evaluated) %>% 
      summarise(number=n(), percent=100*n()/n.calls)
  
  nas <-
    calls %>% left_join(promise_associations, by="call_id") %>% 
    filter(is.na(promise_id)) %>% 
    count() %>% rename(number=n) %>%
    collect %>% ungroup() %>%
    mutate(strictness_ratio="0/0", percent=100*number/n.calls, evaluated=0, promises=0)
  
  rbind(nas %>% as.data.frame, histogram %>% as.data.frame) %>% 
    arrange(promises, evaluated) %>% 
    select(strictness_ratio, number, percent, -promises, -evaluated) 
}

get_call_strictness_rate <- function(functions, calls, promise_associations, promise.forces, n.calls) {
  classification_table <- c( "0", 
    "(0,10〉", "(10,20〉", "(20,30〉", 
    "(30,40〉", "(40,50〉", "(50,60〉", 
    "(60,70〉", "(70,80〉", "(80,90〉", 
    "(90,100)", "100")
  
  classify <- function(percentage)
    ifelse(percentage == 0, 
           classification_table[1], 
           ifelse(percentage == 100, 
                  classification_table[12],        
                  classification_table[((percentage - 1) %/% 10 + 2)]))
  
  histogram <- 
    calls %>% 
    left_join(functions, by="function_id") %>% 
    filter(type==0) %>%
    left_join(promise_associations, by="call_id") %>% 
    filter(!is.na(promise_id)) %>%
    left_join(promise.forces, by="promise_id") %>% 
    group_by(call_id) %>% 
    summarise(
      evaluated=sum(as.integer(!is.na(event_type) && (lifestyle != 3))), 
      count=n()) %>%
    collect %>% ungroup() %>%
    mutate(
      strictness_rate_percent=(100*evaluated/count),
      strictness_rate=classify(strictness_rate_percent)) %>%
    group_by(strictness_rate) %>% 
    summarise(number=n(), percent=100*n()/n.calls)
  
  complete_histogram <-
    data.frame(strictness_rate=classification_table) %>% 
    left_join(histogram, by="strictness_rate") %>% 
    collect %>% ungroup() %>%
      mutate(
        number=ifelse(is.na(number), 0, number), 
        percent=ifelse(is.na(percent), 0, percent))
  
  nas <-
    calls %>% left_join(promise_associations, by="call_id") %>% 
    filter(is.na(promise_id)) %>% 
    count() %>% rename(number=n) %>%
    collect %>% ungroup() %>%
    mutate(strictness_rate=NA, percent=100*number/n.calls)
  
  rbind(complete_histogram, nas)
}

get_function_strictness_aggregate_over_functions <- function(histogram, n.functions)
  histogram %>%
    group_by(function_id) %>% summarise(evaluated=sum(evaluated), count=sum(count)) %>%
    mutate(strict=(evaluated==count)) %>%
    group_by(function_id, strict) %>% summarise() %>%
    group_by(strict) %>% summarise(number=n(), percent=(100*n()/n.functions)) %>% as.data.frame

get_function_strictness <- function(calls, functions, promise_associations, promise.forces, n.functions, dont_aggregate_over_function_ids=FALSE) {
  
  nas <-
    calls %>% 
    left_join(functions, by="function_id") %>% 
    left_join(promise_associations, by="call_id") %>% 
    #filter() %>% FIXME filter out non-closures
    filter(is.na(promise_id)) %>% 
    collect() %>%
    group_by(function_id) %>%
    summarise(type=unique(type), evaluated=NA, count=NA)
  
  histogram <- 
    calls %>% 
    left_join(functions, by="function_id") %>% 
    left_join(promise_associations, by="call_id") %>% 
    filter(!is.na(promise_id)) %>%
    left_join(promise.forces, by="promise_id") %>%
    group_by(call_id, function_id) %>% 
    collect %>%
    summarise(
      type=unique(type),
      evaluated=sum(as.integer(!is.na(event_type) && (lifestyle != 3))), 
      count=n()) %>%
    ungroup() %>%
    group_by(function_id) %>% summarise(type=unique(type), evaluated=sum(evaluated), count=sum(count))

  rbind(histogram %>% data.frame, nas %>% data.frame)
}

get_function_strictness_rate <- function(histogram, n.functions) {
  classification_table <- tibble(
      strictness_rate=c("0", 
      "(0,10〉", "(10,20〉", "(20,30〉", 
      "(30,40〉", "(40,50〉", "(50,60〉", 
      "(60,70〉", "(70,80〉", "(80,90〉", 
      "(90,100)", "100"))
  
  classify <- function(percentage)
    ifelse(percentage == 0, 
           classification_table$strictness_rate[1], 
           ifelse(percentage == 100, 
                  classification_table$strictness_rate[12],        
                  classification_table$strictness_rate[((percentage - 1) %/% 10 + 2)]))

  histogram <-
    histogram %>%
    filter(type==0) %>%
    group_by(function_id) %>% summarise(evaluated=sum(evaluated), count=sum(count)) %>%
    mutate(strictness_rate=classify((100*evaluated/count))) %>%
    group_by(strictness_rate) %>%
    summarise(number=n(), percent=100*n()/n.functions)

  histogram <-
    classification_table %>%
    left_join(histogram, by="strictness_rate") %>% 
      mutate(
        number=ifelse(is.na(number), 0, number), 
        percent=ifelse(is.na(percent), 0, percent)) %>% 
    select(strictness_rate, number, percent) %>%
    as.data.frame

  histogram
}


get_call_strictness_by_type <- function(calls, functions, promise_associations, promise.forces, n.calls) {
  histogram <- 
    calls %>% left_join(functions, by="function_id") %>% 
    left_join(promise_associations, by="call_id") %>% 
    filter(!is.na(promise_id)) %>%
    left_join(promise.forces, by="promise_id") %>% 
    group_by(call_id,type) %>% summarise(
      unevaluated=sum(as.integer(is.na(event_type))), 
      escaped=sum(as.integer(!is.na(event_type) && (lifestyle == 3))), 
      evaluated=sum(as.integer(!is.na(event_type) && (lifestyle != 3))), 
      count=n()) %>%
    mutate(strict=(evaluated==count)) %>%
    group_by(strict, type) %>% 
    summarise(number=n()) %>%
    mutate(percent=100*number/n.calls) %>%
    collect
  
  nas <-
    calls %>% left_join(functions, by="function_id") %>% 
    left_join(promise_associations, by="call_id") %>% 
    filter(is.na(promise_id)) %>% 
    group_by(type) %>% count() %>% rename(number=n) %>%
    mutate(strict=NA) %>%
    mutate(percent=100*number/n.calls) %>%
    collect
  
  intermediate <- 
       rbind(histogram %>% data.frame, nas %>% data.frame)
  
  complete_histogram <-
    merge( # cartesian product
       data.frame(type=intermediate$type %>% unique), 
       data.frame(strict=intermediate$strict %>% unique), 
       by=NULL) %>%
    left_join(intermediate, by=c("type", "strict")) %>%
    mutate(
      number=ifelse(is.na(number), 0, number),
      percent=ifelse(is.na(percent), 0, percent)) %>%
    mutate(type=humanize_function_type(type))
  
  complete_histogram
}

get_function_strictness_by_type <- function(histogram, n.functions) {
  histogram <- 
    histogram %>%
    group_by(function_id) %>% summarise(type=unique(type), evaluated=sum(evaluated), count=sum(count)) %>%
    collect %>% ungroup %>%
    mutate(strict=(evaluated==count)) %>%
    group_by(function_id, type, strict) %>% summarise() %>%
    group_by(strict, type) %>%
    summarise(
      number=n(), 
      percent=100*n()/n.functions)
  
  merge( # cartesian product
     data.frame(type=unique(histogram$type)), 
     data.frame(strict=unique(histogram$strict)), 
     by=NULL) %>%
  left_join(histogram, by=c("type", "strict")) %>%
  mutate(
    number=ifelse(is.na(number), 0, number),
    percent=ifelse(is.na(percent), 0, percent)) %>%
  mutate(type=humanize_function_type(type))
}

get_call_promise_evaluation_order <- function(calls, promise_associations, promises, arguments, promise_evaluations) {
  data <- calls %>% #filter(call_id==4) %>%
    left_join(promise_associations, by="call_id") %>% 
    left_join(promises %>% rename(promise_id = id), by="promise_id") %>% 
    left_join(arguments %>% rename(argument_id=id), by=c("argument_id", "call_id")) %>% 
    left_join(promise_evaluations, by="promise_id") %>%
    select(call_id, promise_id, argument_id, function_id, from_call_id, clock, event_type, name, position) %>%
    collect %>%
    arrange(clock)
  
  force_signatures <- data %>%
    filter(event_type == 15) %>% 
    group_by(call_id) %>%
    summarise(
      force_order = paste(position, collapse="→"))
  
  calls %>% select(call_id) %>% collect %>% 
    left_join(force_signatures, by = "call_id")
}

get_function_promise_evaluation_signatures <- function(functions, calls, call_promise_evaluation_order) {
  functions %>% select(function_id) %>% collect %>%
  left_join(call_promise_evaluation_order %>% 
            left_join(calls %>% 
                      select(call_id, function_id) %>% 
                      collect, 
                      by="call_id"), 
            by="function_id") %>% 
  filter(!is.na(force_order))
}

get_function_promise_evaluation_order_summary <- function(functions, unique_signatures) {    
  function_list <- functions %>% select(function_id) %>% collect
  
  unique_signatures %>%
    group_by(function_id) %>% 
    summarise(force_orders=length(unique(force_order)), calls=n()) %>%
    right_join(function_list, by ="function_id") %>%
    collect
}

# FIXME returns NA for calls and percent.calls for no.of.force.orders==0
get_function_promise_force_order_histogram <- function(function_promise_evaluation_order, n.functions, n.calls, cutoff=NA) {
  data <- 
    function_promise_evaluation_order %>% 
    group_by(force_orders) %>% 
    summarise(
      number=n(),
      calls=sum(calls))%>% 
    rename(no.of.force.orders=force_orders) %>% 
    mutate(
      percent=(number*100/n.functions),
      percent.calls=(calls*100/n.calls))
  
  nas <- data %>% filter(is.na(no.of.force.orders)) %>% ungroup %>% mutate(no.of.force.orders = 0)
  new <- data %>% filter(!is.na(no.of.force.orders)) %>% arrange(no.of.force.orders)
    
  { if (is.na(cutoff) || max(data$no.of.force.orders, na.rm=TRUE) <= cutoff) {
    rbind(nas %>% as.data.frame, new %>% as.data.frame)
  } else {
    above <- new %>% filter(no.of.force.orders > cutoff) %>% ungroup %>% collect %>% summarise(no.of.force.orders=Inf, number=sum(number), calls=sum(calls), percent=(number*100/n.functions), percent.calls=(calls*100/n.calls))
    below <- new %>% filter(no.of.force.orders <= cutoff)
    rbind(nas %>% as.data.frame, below %>% as.data.frame, above %>% as.data.frame) 
  } } %>%
    mutate(no.of.force.orders=
           ifelse(is.na(no.of.force.orders), 
                  0,
                  ifelse(no.of.force.orders==Inf,
                         paste(">",cutoff, sep=""),
                         as.character(no.of.force.orders))))
}

get_function_calls <- function(functions, calls, n.calls, ...) {
  patterns <- list(...)
  data <- 
    left_join(functions, calls, by="function_id") %>% 
    select(function_name, function_id) %>% 
    #distinct(function_name, function_id) %>% 
    group_by(function_name, function_id) %>% count() %>% rename(number = n) %>%
    mutate(percent = ((number * 100) / n.calls)) %>%
    mutate(function_name = ifelse(is.na(function_name), "<anonymous>", function_name)) %>%
    select(function_id, function_name, number, percent) %>%
    collect(n=Inf)
    lapply(patterns, function(pattern) {filter(data, grepl(pattern, function_name))} ) %>% bind_rows
  data
}

