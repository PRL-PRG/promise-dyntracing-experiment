DATA_DIR=data/2017-11-06-13-41-34
OUTPUT_DIR=output

TRACER=../R-dyntrace/bin/R
PROCESSES=1
PACKAGES=

VANILLA_R=R
VANILLA_RSCRIPT=Rscript

trace: 
	dyntrace/packages.sh $(TRACER) $(DATA_DIR) $(PROCESSES) $(PACKAGES)

check:
	dyntrace/check_results.sh $(DATA_DIR)

get-todo-packages:
	dyntrace/get_todo_list.sh $(DATA_DIR)

clean:
	dyntrace/clean_results.sh $(DATA_DIR)

csvs:
	graphs/generate_data.sh $(DATA_DIR)/data/*.sqlite

aggregate-csvs:
	if [ -e $(DATA_DIR)/csv/_all ]; then rm -ri $(DATA_DIR)/csv/_all; fi
	graphs/aggregate_csvs.sh $(DATA_DIR)/csv/_all $(DATA_DIR)/csv/*

report:
	$(VANILLA_RSCRIPT) -e "rmarkdown::render('graphs/report.Rmd', params=list(CSV_DIR=$(DATA_DIR)/csv, PRINT_DATA=TRUE))"

analyze-argument-promise-mode:
	mkdir -p $(OUTPUT_DIR)/argument-promise-mode/table
	mkdir -p $(OUTPUT_DIR)/argument-promise-mode/graph
	analysis/argument-promise-mode.R $(DATA_DIR) $(OUTPUT_DIR)/argument-promise-mode/table $(OUTPUT_DIR)/argument-promise-mode/graph

analyze-environment:
	mkdir -p $(OUTPUT_DIR)/environment/table
	mkdir -p $(OUTPUT_DIR)/environment/graph
	analysis/environment.R $(DATA_DIR) $(OUTPUT_DIR)/environment/table $(OUTPUT_DIR)/environment/graph

analyze-argument-position-laziness:
	mkdir -p $(OUTPUT_DIR)/argument-position-laziness/table
	mkdir -p $(OUTPUT_DIR)/argument-position-laziness/graph
	analysis/argument-position-laziness $(INPUT_DIR) $(OUTPUT_DIR)/argument-position-laziness/table $(OUTPUT_DIR)/argument-position-laziness/graph

analyze-side-effects:
	mkdir -p $(OUTPUT_DIR)/side-effects/table
	mkdir -p $(OUTPUT_DIR)/side-effects/graph
	analysis/side-effects $(INPUT_DIR) $(OUTPUT_DIR)/side-effects/table $(OUTPUT_DIR)/side-effects/graph

analyze-promise-memory-usage:
	mkdir -p $(OUTPUT_DIR)/promise-memory-usage/table
	mkdir -p $(OUTPUT_DIR)/promise-memory-usage/graph
	analysis/promise-memory-usage.R $(INPUT_DIR) $(OUTPUT_DIR)/promise-memory-usage/table $(OUTPUT_DIR)/promise-memory-usage/graph

analyze: analyze-environment analyze-argument-promise-mode analyze-argument-position-laziness analyze-promise-memory-usage
