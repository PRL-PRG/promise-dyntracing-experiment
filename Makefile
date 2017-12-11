DATA_DIR=data/2017-11-06-13-41-34
ANALYSIS_DIR=output

TRACER=../R-dyntrace/bin/R
PROCESSES=1
PACKAGES=

VANILLA_R=R
VANILLA_RSCRIPT=Rscript

trace: 
	dyntrace/packages.sh $(TRACER) $(DATA_DIR) $(PROCESSES) $(PACKAGES)

check-data:
	dyntrace/check_results.sh $(DATA_DIR)

get-todo-packages:
	dyntrace/get_todo_list.sh $(DATA_DIR)

clean-data:
	dyntrace/clean_results.sh $(DATA_DIR)

make-csv:
	graphs/generate_data.sh $(DATA_DIR)/data/*.sqlite
	
make-report:
	$(VANILLA_RSCRIPT) -e "rmarkdown::render('graphs/report.Rmd', params=list(CSV_DIR=$(DATA_DIR)/csv, PRINT_DATA=TRUE))"
	
analyze-argument-promise-mode:
	mkdir -p $(OUTPUT_DIR)/argument-promise-mode/table
	mkdir -p $(OUTPUT_DIR)/argument-promise-mode/graph
	analysis/argument-promise-mode.R $(DATA_DIR) $(OUTPUT_DIR)/argument-promise-mode/table $(OUTPUT_DIR)/argument-promise-mode/graph

analyze-environment:
	mkdir -p $(OUTPUT_DIR)/environment/table
	mkdir -p $(OUTPUT_DIR)/environment/graph
	analysis/environment.R $(DATA_DIR) $(OUTPUT_DIR)/environment/table $(OUTPUT_DIR)/environment/graph

analyze: analyze-environment analyze-argument-promise-mode
