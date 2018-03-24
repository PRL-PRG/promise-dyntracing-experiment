#suppressPackageStartupMessages(library("optparse"))
source("graphs/generate_data.R")

# for (arg in commandArgs(trailingOnly=TRUE)) {
#  #arg <- "/home/kondziu/workspace/R-dyntrace/data/rivr.sqlite"
#  name <- gsub("\\..*$", "", basename(arg))
#  if (length(commandArgs(trailingOnly=TRUE) > 1)) write(name, stderr())
#  main(name, arg, Sys.getenv("CSV_DIR"), debug=TRUE)
# }

preaggregated <- function(database_path, output_path, debug = TRUE) {
  load <- load(input_path=output_path)
  store <- store(name="_all", output_path=output_path)
  dir.create(output_path, recursive=TRUE, showWarnings=FALSE)  
  
  db <- src_sqlite(database_path)
  functions <- db %>% tbl("functions") %>% rename(function_id = id) %>% mutate(definition=gsub("\n","%\\n ",definition) %>% collect # TODO rename to unaggregated_
  store("aggregated_functions")(functions)
  
  basic_info <- load("basic_info")
  
  total_functions <- functions %>% count %>% pull(n)
  store("aggregated_function_types")(get_functions_by_type(functions, total_functions))
  
  unaggregated_compiled_functions <- load("unaggregated_compiled_functions")
  store("aggregated_compiled_functions")(get_function_compilations_by_type_aggregate_over_functions(unaggregated_compiled_functions, functions, total_functions, "closure"))
  
  unaggregated_function_strictness <- load("unaggregated_function_strictness")
  store("aggregated_function_strictness")(get_function_strictness_aggregate_over_functions(unaggregated_function_strictness, total_functions))
  store("aggregated_function_strictness_rate")(get_function_strictness_rate(unaggregated_function_strictness, total_functions))
  store("aggregated_function_strictness_by_type")(get_function_strictness_by_type(unaggregated_function_strictness, total_functions))
  
  unaggregated_evaluation_order <- load("unaggregated_evaluation_order")
  store("aggregated_evaluation_order")(
    get_function_promise_evaluation_order_summary(functions, unaggregated_evaluation_order) %>%
    get_function_promise_force_order_histogram(n.functions=total_functions, n.calls=sum(basic_info$n.calls), cutoff=10)
  )
}

for (arg in commandArgs(trailingOnly=TRUE)) {
  preaggregated(arg, Sys.getenv("CSV_DIR"), debug=TRUE)
}