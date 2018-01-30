INPUT_DIR=data/2017-11-06-13-41-34
OUTPUT_DIR=output

TRACER=../R-dyntrace/bin/R
PROCESSES=1
PACKAGES=

VANILLA_R=R
VANILLA_RSCRIPT=Rscript

trace:
	dyntrace/packages.sh $(TRACER) $(INPUT_DIR) $(PROCESSES) $(PACKAGES)

check:
	dyntrace/check_results.sh $(INPUT_DIR)

get-todo-packages:
	dyntrace/get_todo_list.sh $(INPUT_DIR)

clean:
	dyntrace/clean_results.sh $(INPUT_DIR)

csvs:
	graphs/generate_data.sh $(INPUT_DIR)/data/*.sqlite

aggregate-csvs:
	if [ -e $(INPUT_DIR)/csv/_all ]; then rm -ri $(INPUT_DIR)/csv/_all; fi
	graphs/aggregate_csvs.sh $(INPUT_DIR)/csv/_all $(INPUT_DIR)/csv/*

conglomerate-csvs:
	Rscript graphs/conglomerate_csvs.R $(INPUT_DIR)/csv/_all

report:
	$(VANILLA_RSCRIPT) -e "rmarkdown::render('graphs/report.Rmd', params=list(CSV_DIR='$(INPUT_DIR)/csv/_all', PRINT_DATA=TRUE))"

analyze-argument-promise-mode:
	mkdir -p $(OUTPUT_DIR)/argument-promise-mode/table
	mkdir -p $(OUTPUT_DIR)/argument-promise-mode/graph
	analysis/argument-promise-mode.R $(INPUT_DIR) $(OUTPUT_DIR)/argument-promise-mode/table $(OUTPUT_DIR)/argument-promise-mode/graph

analyze-environment:
	mkdir -p $(OUTPUT_DIR)/environment/table
	mkdir -p $(OUTPUT_DIR)/environment/graph
	analysis/environment.R $(INPUT_DIR) $(OUTPUT_DIR)/environment/table $(OUTPUT_DIR)/environment/graph

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

analyze-promise-lifespan:
	mkdir -p $(OUTPUT_DIR)/promise-lifespan/table
	mkdir -p $(OUTPUT_DIR)/promise-lifespan/graph
	analysis/promise-lifespan.R $(INPUT_DIR) $(OUTPUT_DIR)/promise-lifespan/table $(OUTPUT_DIR)/promise-lifespan/graph

analyze: analyze-environment analyze-argument-promise-mode analyze-argument-position-laziness analyze-promise-memory-usage analyze-promise-lifespan
