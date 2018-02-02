#suppressPackageStartupMessages(library("optparse"))
source("graphs/generate_data.R")

main <- function(package, database_path, output_path, debug = TRUE) {
  
  # The database
  db <- src_sqlite(database_path)
  store <- store(name=package, output_path=output_path)
  
  # Output
  dir.create(output_path, recursive=TRUE, showWarnings=FALSE)
  
  # Tables in the DB
  promises <- db %>% tbl("promises")
  promise_evaluations <- db %>% tbl("promise_evaluations")
  promise_associations <- db %>% tbl("promise_associations")
  promise_returns <- db %>% tbl("promise_returns")
  calls <- db %>% tbl("calls") %>% rename(call_id = id)
  functions <- db %>% tbl("functions") %>% rename(function_id = id)
  arguments <- db %>% tbl("arguments")
  metadata <- db %>% tbl("metadata")
  
  # Helper tables
  promise.forces <- promise_evaluations %>% filter(promise_id >= 0 && event_type == 15)
  promise.lookups <- promise_evaluations %>% filter(promise_id >= 0 && event_type == 0)
  alien.promise.forces <- promise_evaluations %>% filter(promise_id < 0 && event_type == 15)
  
  # Overview stats
  n.functions <- (functions %>% count %>% data.frame)$n
  n.calls <- (calls %>% count %>% data.frame)$n
  n.promises <- (promises %>% count %>% data.frame)$n
  n.alien.promises <- (promise_evaluations %>% filter(promise_id < 0) %>% 
                         group_by(promise_id) %>% count %>% data.frame)$promise_id %>% length
  n.promise.forces <- (promise.forces %>% count %>% data.frame)$n
  n.promise.lookups <- (promise.lookups %>% count %>% data.frame)$n 
  n.alien.promise.forces <- (alien.promise.forces %>% count %>% data.frame)$n
  n.alien.promise.lookups <- NA # I currently don't collect this information to save space
  
  # Write overview stats to file
  store("basic_info")(
    path = database_path,
    n.functions = n.functions,
    n.calls = n.calls,
    n.promises = n.promises,
    n.promise.forces = n.promise.forces,
    n.promise.lookups = n.promise.lookups,
    n.alien.promises = n.alien.promises,
    n.alien.promise.forces = n.alien.promise.forces,
    n.alien.promise.lookups = n.alien.promise.lookups
  )
  
  # Write metadata to file
  store("metadata")(metadata %>% distinct %>% as.data.frame)
  
  # Promise evaluation
  store("forces")(get_forces(promises, promise.forces, n.promises))
  store("fuzzy_forces")(get_fuzzy_forces(promises, promise_evaluations, n.promises))
  store("promise_evaluations")(get_promise_evaluations(promises, promise_evaluations, n.promises, cutoff=10))
  store("promises_forced_by_another_promise")(get_promises_forced_by_another_evaluations(promises, promise.forces, n.promises))
  store("promises_forcing_other_promises")(get_cascading_promises(promises, promise.forces))
  
  # Promise types
  store("promise_types")(get_promise_types(promises, n.promises, cutoff=NA))
  store("promise_full_types")(get_full_promise_types(promises, n.promises, cutoff=NA))
  store("return_types")(get_promise_return_types(promises, promise_returns, n.promises, cutoff=NA))
  
  store("promise_code_to_return_types")(get_promise_types_to_return_types(promises, promise_returns, n.promises, cutoff=NA))
  store("forces_by_type")(get_forces_by_type(promises, promise.forces, n.promises, n.promise.forces))
  
  # Evaluation distances
  store("actual_distances")(get_actual_distances(promises, promise.forces, n.promise.forces, cutoff=20))
  
  # Functions and calls
  store("call_types")(get_calls_by_type(calls, functions, n.calls))
  store("compiled_calls")(get_call_compilations_by_type(calls, functions, n.calls, "closure"))
  
  store("function_types")(get_functions_by_type(functions, n.functions))

  unaggregated_compiled_functions <- get_function_compilations_by_type_actual(calls, functions, n.functions, "closure", dont_aggregate_over_function_ids=TRUE)
  store("unaggregated_compiled_functions")(unaggregated_compiled_functions)
  store("compiled_functions")(get_function_compilations_by_type_aggregate_over_functions(unaggregated_compiled_functions, functions, n.functions, "closure"))
  
  # Strictness
  store("call_strictness")(get_call_strictness(calls, promise_associations, promise.forces, n.calls))
  store("call_strictness_rate")(get_call_strictness_rate(functions, calls, promise_associations, promise.forces, n.calls))
  store("call_strictness_ratio")(get_call_strictness_ratios(functions, calls, promise_associations, promise.forces, n.calls))
  store("call_strictness_by_type")(get_call_strictness_by_type(calls, functions, promise_associations, promise.forces, n.calls))
  
  unaggregated_function_strictness <- get_function_strictness(calls, functions, promise_associations, promise.forces, n.functions, dont_aggregate_over_function_ids=TRUE)
  store("unaggregated_function_strictness")(unaggregated_function_strictness)
  store("function_strictness")(get_function_strictness_aggregate_over_functions(unaggregated_function_strictness, n.functions))
  store("function_strictness_rate")(get_function_strictness_rate(unaggregated_function_strictness, n.functions))
  store("function_strictness_by_type")(get_function_strictness_by_type(unaggregated_function_strictness, n.functions))
  
  unaggregated_evaluation_order <- 
    get_call_promise_evaluation_order(calls, promise_associations, promises, arguments, promise_evaluations) %>%
    get_function_promise_evaluation_signatures(functions=functions, calls=calls)
    
  store("unaggregated_evaluation_order")(unaggregated_evaluation_order)  
  store("evaluation_order")(
    get_function_promise_evaluation_order_summary(functions, unaggregated_evaluation_order) %>%
    get_function_promise_force_order_histogram(n.functions=n.functions, n.calls=n.calls, cutoff=10)
  )
  
  # Specific calls
  store("specific_calls")(
    get_function_calls(functions, calls, n.calls, 
                       "^<anonymous>$", "eval", "force", "delayAssign", 
                       "return", "<-", "assign", "print") %>% 
      select(function_name, number, percent))
}

for (arg in commandArgs(trailingOnly=TRUE)) {
 #arg <- "/home/kondziu/workspace/R-dyntrace/data/rivr.sqlite"
 name <- gsub("\\..*$", "", basename(arg))
 if (length(commandArgs(trailingOnly=TRUE) > 1)) write(name, stderr())
 main(name, arg, Sys.getenv("CSV_DIR"), debug=TRUE)
}

#preaggregated("/media/kondziu/b7a9548c-13a0-4fe6-8a28-de8d491a209b/R/traces/lucoa/2018-01-19-18-30-19/data/_all/functions.sqlite", Sys.getenv("CSV_DIR"), debug=TRUE)