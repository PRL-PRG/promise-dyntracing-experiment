suppressPackageStartupMessages(library("dplyr"))
suppressPackageStartupMessages(library("tidyr"))

CSV_DIR=commandArgs(trailingOnly=TRUE)[1] #paste(, "csv/_all/", sep="/")

load_csv <- function (filename) read.csv(paste(CSV_DIR, "/", filename, ".csv", sep=""))

pp <- function(number, justify="none") format(number, big.mark=",", scientific=FALSE, trim=FALSE, format="f", justify=justify, digits=1)

pp_and_perc <- function(number, percent) paste0(pp(number), " = ", pp(percent), "%")
  

basic_info <- load_csv("basic_info")
metadata <- load_csv("metadata")
promise_evaluations <- load_csv("promise_evaluations")
forces <- load_csv("forces")
fuzzy_forces <- load_csv("fuzzy_forces")
forces_by_type <- load_csv("forces_by_type")
promise_types <- load_csv("promise_types")
return_types <- load_csv("return_types")
promise_code_to_return_types <- load_csv("promise_code_to_return_types")
promise_evaluations <- load_csv("promise_evaluations")
promises_forced_by_another_promise <- load_csv("promises_forced_by_another_promise")
promises_forcing_other_promises <- load_csv("promises_forcing_other_promises")
actual_distances <- load_csv("actual_distances")   
call_types <- load_csv("call_types")
function_types <- load_csv("function_types")
compiled_calls <- load_csv("compiled_calls")
print("x")
compiled_functions <- load_csv("compiled_functions") # FIXME will not work on non-distinct data
print("y")
evaluation_order <- load_csv("evaluation_order")
print("z")
call_strictness <- load_csv("call_strictness")
print("1")
function_strictness <- load_csv("function_strictness")
print("2")
call_strictness_by_type <- load_csv("call_strictness_by_type")
print("3")
function_strictness_by_type <- load_csv("function_strictness_by_type")
print("4")
call_strictness_ratio <- load_csv("call_strictness_ratio")
print("5")
function_strictness_rate <- load_csv("function_strictness_rate")
print("6")

##############################

#call_strictness_rate <- load_csv("call_strictness_rate")
#promise_full_types <- load_csv("promise_full_types")
#specific_calls <- load_csv("specific_calls")

########################################

flat_basic_info <- basic_info %>% distinct
flat_metadata <- metadata %>% distinct %>% spread(key, value)

flat_forces <- forces %>% distinct %>% mutate(number = pp_and_perc(number, percent)) %>% select(name, no.of.forces, number) %>% rename("promise forced times"=no.of.forces) %>% spread("promise forced times",  number, sep=" ", fill=pp_and_perc(0,0))
flat_fuzzy_forces <- fuzzy_forces %>% distinct %>% mutate(number = pp_and_perc(number, percent)) %>% select(name, classification, number) %>% rename(promises=classification) %>% spread(promises,  number, sep=" ", fill=pp_and_perc(0,0))
flat_forces_by_type <- forces_by_type %>% distinct %>% mutate(number = paste0(pp(number), " = ", pp(percent_within_type), "% within ", type, " = ", pp(percent_overall), "% overall")) %>% mutate("promises of type"=paste0(type, " forced ", no.of.forces, " times")) %>% select(name, "promises of type", number) %>% spread("promises of type",  number, sep=" ", fill = pp_and_perc(0,0))

flat_promise_types <- promise_types %>% distinct %>% mutate(number = pp_and_perc(number, percent)) %>% select(name, type, number) %>% rename(promise=type) %>% spread(promise,  number, fill=pp_and_perc(0,0), sep=" is ")
flat_return_types <- return_types %>% distinct %>% mutate(number = pp_and_perc(number, percent)) %>% select(name, type, number) %>% rename(promise=type) %>% spread(promise,  number, fill=pp_and_perc(0,0), sep=" returns ")
flat_promise_code_to_return_types <- promise_code_to_return_types %>% distinct %>% mutate(number = pp_and_perc(number, percent)) %>% select(name, types, number) %>% rename(promise=types) %>% spread(promise,  number, fill=pp_and_perc(0,0), sep=" is ")

flat_promise_evaluations <- promise_evaluations %>% distinct %>% mutate(number = pp_and_perc(number, percent)) %>% select(name, no.of.evaluations, number) %>% rename("promises evaluated times"=no.of.evaluations) %>% spread("promises evaluated times",  number, sep=" ", fill=pp_and_perc(0,0))

flat_promise_forced_by_another_promise <- promises_forced_by_another_promise %>% distinct %>% mutate(number = pp_and_perc(number, percent)) %>% select(name, forced_by_another, number) %>% rename("promise forced by another"=forced_by_another) %>% spread("promise forced by another",  number, sep="=", fill=pp_and_perc(0,0))
flat_promise_forcing_other_promises <- promises_forcing_other_promises %>% distinct %>% mutate(number = pp_and_perc(number, percent)) %>% select(name, number_of_forced_promises, number) %>% rename("promise forced promises"=number_of_forced_promises) %>% spread("promise forced promises",  number, sep="=", fill=pp_and_perc(0,0))

flat_actual_distances <- actual_distances %>% distinct %>% mutate(number = pp_and_perc(number, percent)) %>% select(name, actual_distance, number) %>% rename("distance from creation to force"=actual_distance) %>% spread("distance from creation to force",  number, sep="=", fill=pp_and_perc(0,0))

flat_call_types <- call_types %>% distinct %>% mutate(number = pp_and_perc(number, percent)) %>% select(name, type, number) %>% rename(calls=type) %>% spread(calls,  number, sep="=", fill=pp_and_perc(0,0))
flat_function_types <- function_types %>% distinct %>% mutate(number = pp_and_perc(number, percent)) %>% select(name, type, number) %>% rename(functions=type) %>% spread(functions, number, sep="=", fill=pp_and_perc(0,0))

flat_compiled_calls <- compiled_calls %>% distinct %>% mutate(number = pp_and_perc(number, percent)) %>% select(name, compiled, number) %>% rename(calls=compiled) %>% spread(calls, number, sep="=", fill=pp_and_perc(0,0))
#flat_compiled_functions <- compiled_functions %>% distinct %>% mutate(number = pp_and_perc(number, percent)) %>% select(name, compiled, number) %>% rename(functions=compiled) %>% spread(functions, number, sep="=", fill=pp_and_perc(0,0))

flat_evaluation_order <- evaluation_order %>%  distinct %>% mutate(number = pp_and_perc(number, percent)) %>% select(name, no.of.force.orders, number) %>% rename("function with number of force orders"=no.of.force.orders) %>% spread("function with number of force orders", number, sep=" = ", fill=pp_and_perc(0,0))
#evaluation_order %>%  distinct %>% mutate(number = pp_and_perc(calls, percent.calls)) %>% select(name, no.of.force.orders, number) %>% rename("calls in functions with number of force orders"=no.of.force.orders) %>% spread("calls in functions with number of force orders", number, sep=" = ", fill=pp_and_perc(0,0))

flat_call_strictness <- call_strictness %>%  distinct %>% mutate(number = pp_and_perc(number, percent)) %>% select(name, strict, number) %>% rename(calls=strict) %>% spread(calls,  number, sep=" strict=", fill=pp_and_perc(0,0))
flat_function_strictness <- function_strictness %>%  distinct %>% mutate(number = pp_and_perc(number, percent)) %>% select(name, strict, number) %>% rename(functions=strict) %>% spread(functions,  number, sep=" strict=", fill=pp_and_perc(0,0))

flat_call_strictness_by_type <- call_strictness_by_type %>% distinct %>% mutate(strict=as.logical(strict)) %>% mutate(number = pp_and_perc(number, percent), strict=paste0(type, " and strict=", strict)) %>% select(name, strict, number) %>% rename(call=strict) %>% spread(call,  number, sep=" is ", fill=pp_and_perc(0,0))
flat_function_strictness_by_type <- function_strictness_by_type %>% distinct %>% mutate(strict=as.logical(strict)) %>% mutate(number = pp_and_perc(number, percent), strict=paste0(type, " and strict=", strict)) %>% select(name, strict, number) %>% rename("function"=strict) %>% spread("function",  number, sep=" is ", fill=pp_and_perc(0,0))

flat_call_strictness_ratio <- call_strictness_ratio %>% distinct %>% mutate(number = pp_and_perc(number, percent)) %>% select(name, strictness_ratio, number) %>% rename("call strictness ratio"=strictness_ratio) %>% spread("call strictness ratio",  number, sep=" ", fill=pp_and_perc(0,0))
flat_function_strictness_ratio <- function_strictness_rate %>% distinct %>% mutate(number = pp_and_perc(number, percent)) %>% select(name, strictness_rate, number) %>% rename("function strictness rate"=strictness_rate) %>% spread("function strictness rate",  number, sep=" ", fill=pp_and_perc(0,0))

############################################

conglomeration <- 
  flat_basic_info %>%
  left_join(flat_metadata, by="name") %>%
  left_join(flat_forces, by="name") %>%  
  left_join(flat_fuzzy_forces, by="name") %>%  
  left_join(flat_forces_by_type, by="name") %>%  
  left_join(flat_promise_types, by="name") %>%  
  left_join(flat_return_types, by="name") %>%  
  left_join(flat_promise_code_to_return_types, by="name") %>%  
  left_join(flat_promise_evaluations, by="name") %>%  
  left_join(flat_promise_forced_by_another_promise, by="name") %>%  
  left_join(flat_promise_forcing_other_promises, by="name") %>%  
  left_join(flat_actual_distances, by="name") %>%  
  left_join(flat_call_types, by="name") %>%  
  left_join(flat_function_types, by="name") %>%  
  left_join(flat_compiled_calls, by="name") %>%  
  #left_join(flat_compiled_functions, by="name") %>%  
  left_join(flat_evaluation_order, by="name") %>%  
  left_join(flat_call_strictness, by="name") %>%  
  left_join(flat_function_strictness, by="name") %>%  
  left_join(flat_call_strictness_by_type, by="name") %>%  
  left_join(flat_function_strictness_by_type, by="name") %>%  
  left_join(flat_call_strictness_ratio, by="name") %>%  
  left_join(flat_function_strictness_ratio, by="name")

write.csv(conglomeration, file=file.path(CSV_DIR, "summary.csv"))
