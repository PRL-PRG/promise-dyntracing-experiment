source("graphs/generate_data.R")

main <- function(analysis, package, database_path, output_path, debug = TRUE) {
  # The database
  db <- src_sqlite(database_path)
  store <- store(name=package, output_path=output_path)
  store_exists <- store_exists(name=package, output_path=output_path)
  
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
  
  if(analysis == "overview" && !store_exists("basic_info")) {  
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
    return()
  }
  
  # Write metadata to file
  if (analysis == "metadata" && !store_exists("metadata")) {
    store("metadata")(metadata %>% distinct %>% as.data.frame)
    return()
  }
  
  # Promise evaluation
  if (analysis == "forces" && !store_exists("forces")) {
    n.promises <- (promises %>% count %>% data.frame)$n
    store("forces")(get_forces(promises, promise.forces, n.promises))
    return()
  }
  if (analysis == "fuzzy_forces" && !store_exists("fuzzy_forces")) {
    n.promises <- (promises %>% count %>% data.frame)$n
    store("fuzzy_forces")(get_fuzzy_forces(promises, promise_evaluations, n.promises))
    return()
  }
  if (analysis == "promise_evaluations" && !store_exists("promise_evaluations")) {
    n.promises <- (promises %>% count %>% data.frame)$n
    store("promise_evaluations")(get_promise_evaluations(promises, promise_evaluations, n.promises, cutoff=10))
    return()
  }
  if (analysis == "promises_forced_by_another_promise" && !store_exists("promises_forced_by_another_promise")) {
    n.promises <- (promises %>% count %>% data.frame)$n
    store("promises_forced_by_another_promise")(get_promises_forced_by_another_evaluations(promises, promise.forces, n.promises))
    return()
  }
  if (analysis == "promises_forcing_other_promises" && !store_exists("promises_forcing_other_promises")) {
    store("promises_forcing_other_promises")(get_cascading_promises(promises, promise.forces))
    return()
  }
  
  # Promise types
  if (analysis == "promise_types" && !store_exists("promise_types")) {
    n.promises <- (promises %>% count %>% data.frame)$n
    store("promise_types")(get_promise_types(promises, n.promises, cutoff=NA))
    return()
  }
  if (analysis == "promise_full_types" && !store_exists("promise_full_types")) {
    n.promises <- (promises %>% count %>% data.frame)$n
    store("promise_full_types")(get_full_promise_types(promises, n.promises, cutoff=NA))
    return()
  }
  if (analysis == "return_types" && !store_exists("return_types")) {
    n.promises <- (promises %>% count %>% data.frame)$n
    store("return_types")(get_promise_return_types(promises, promise_returns, n.promises, cutoff=NA))
    return()
  }
  
  if (analysis == "" && !store_exists("promise_code_to_return_types")) {
    n.promises <- (promises %>% count %>% data.frame)$n
    store("promise_code_to_return_types")(get_promise_types_to_return_types(promises, promise_returns, n.promises, cutoff=NA))
    return()
  }
  if (analysis == "forces_by_type" && !store_exists("forces_by_type")) {
    n.promises <- (promises %>% count %>% data.frame)$n
    n.promise.forces <- (promise.forces %>% count %>% data.frame)$n
    store("forces_by_type")(get_forces_by_type(promises, promise.forces, n.promises, n.promise.forces))
    return()
  }
  
  # Evaluation distances
  if (analysis == "actual_distances" && !store_exists("actual_distances")) {
    n.promise.forces <- (promise.forces %>% count %>% data.frame)$n
    store("actual_distances")(get_actual_distances(promises, promise.forces, n.promise.forces, cutoff=20))
    return()
  }
  
  # Functions and calls
  if (analysis == "call_types" && !store_exists("call_types")) {
    n.calls <- (calls %>% count %>% data.frame)$n
    store("call_types")(get_calls_by_type(calls, functions, n.calls))
    return()
  }
  if (analysis == "compiled_calls" && !store_exists("compiled_calls")) {
    n.calls <- (calls %>% count %>% data.frame)$n
    store("compiled_calls")(get_call_compilations_by_type(calls, functions, n.calls, "closure"))
    return()
  }
  
  if (analysis == "function_types" && !store_exists("function_types")) {
    n.functions <- (functions %>% count %>% data.frame)$n
    store("function_types")(get_functions_by_type(functions, n.functions))
    return()
  }

  if (analysis == "compiled_functions" && !store_exists("compiled_functions")) {
    n.functions <- (functions %>% count %>% data.frame)$n
    unaggregated_compiled_functions <- get_function_compilations_by_type_actual(calls, functions, n.functions, "closure", dont_aggregate_over_function_ids=TRUE)
    store("unaggregated_compiled_functions")(unaggregated_compiled_functions)
    store("compiled_functions")(get_function_compilations_by_type_aggregate_over_functions(unaggregated_compiled_functions, functions, n.functions, "closure"))
    return()
  }

  # Strictness
  if (analysis == "call_strictness" && !store_exists("call_strictness")) {
    n.calls <- (calls %>% count %>% data.frame)$n
    store("call_strictness")(get_call_strictness(calls, promise_associations, promise.forces, n.calls))
    return()
  }
  if (analysis == "call_strictness_rate" && !store_exists("call_strictness_rate")) {
    n.calls <- (calls %>% count %>% data.frame)$n
    store("call_strictness_rate")(get_call_strictness_rate(functions, calls, promise_associations, promise.forces, n.calls))
    return()
  }
  if (analysis == "call_strictness_ratio" && !store_exists("call_strictness_ratio")) {
    n.calls <- (calls %>% count %>% data.frame)$n
    store("call_strictness_ratio")(get_call_strictness_ratios(functions, calls, promise_associations, promise.forces, n.calls))
    return()
  }
  if (analysis == "call_strictness_by_type" && !store_exists("call_strictness_by_type")) {
    n.calls <- (calls %>% count %>% data.frame)$n
    store("call_strictness_by_type")(get_call_strictness_by_type(calls, functions, promise_associations, promise.forces, n.calls))
    return()
  }
  
  if (analysis == "function_strictness" && !store_exists("function_strictness_by_type")) {
    n.functions <- (functions %>% count %>% data.frame)$n
    unaggregated_function_strictness <- get_function_strictness(calls, functions, promise_associations, promise.forces, n.functions, dont_aggregate_over_function_ids=TRUE)
    store("unaggregated_function_strictness")(unaggregated_function_strictness)
    store("function_strictness")(get_function_strictness_aggregate_over_functions(unaggregated_function_strictness, n.functions))
    store("function_strictness_rate")(get_function_strictness_rate(unaggregated_function_strictness, n.functions))
    store("function_strictness_by_type")(get_function_strictness_by_type(unaggregated_function_strictness, n.functions))
    return()
  }
  
  if (analysis == "evaluation_order" && !store_exists("evaluation_order")) {
    n.functions <- (functions %>% count %>% data.frame)$n
    n.calls <- (calls %>% count %>% data.frame)$n
    unaggregated_evaluation_order <- 
      get_call_promise_evaluation_order(calls, promise_associations, promises, arguments, promise_evaluations) %>%
      get_function_promise_evaluation_signatures(functions=functions, calls=calls)
      
    store("unaggregated_evaluation_order")(unaggregated_evaluation_order)  
    store("evaluation_order")(
      get_function_promise_evaluation_order_summary(functions, unaggregated_evaluation_order) %>%
      get_function_promise_force_order_histogram(n.functions=n.functions, n.calls=n.calls, cutoff=10)
    )
    return()
  }
  
  # Specific calls
  if (analysis == "specific_calls" && !store_exists("specific_calls")) {
  store("specific_calls")(
    get_function_calls(functions, calls, n.calls, 
                       "^<anonymous>$", "eval", "force", "delayAssign", 
                       "return", "<-", "assign", "print") %>% 
      select(function_name, number, percent))
    return()
  }
}

args <-commandArgs(trailingOnly=TRUE)
if (length(args) > 2 || length(args) == 0) {
  write(paste0("invalid number of arguments, usage:\n",
               "    generate_csvs.R [PACKAGE] [OUTPUT_DIR]\n", 
               "    generate_csvs.R [PACKAGE:OUTPUT_DIR]"), stderr())
  quit(status=1)
}

if (length(args) == 1) { write(args[1],stderr())
  args <- strsplit(args[1], split=":", fixed=TRUE)[[1]]}

analysis <- args[1]
database <- args[2]
csv_dir <- args[3]
write(paste0("Extracting CSVs for ", analysis, "\n",
             "               from ", database, "\n",
             "                 to ", csv_dir), stderr())

if(!dir.exists(csv_dir)) 
  suppressWarnings(dir.create(csv_dir, recursive=TRUE))

name <- gsub("\\..*$", "", basename(database))

main(analysis, name, database, csv_dir, debug=TRUE)
