DATA_DIR=data/2017-11-06-13-41-34

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

concatenate-functions:
	graphs/concat_functions.sh $(DATA_DIR)

conglomerate-csvs:
	Rscript graphs/conglomerate_csvs.R $(DATA_DIR)/csv/_all

report:
	$(VANILLA_RSCRIPT) -e "rmarkdown::render('graphs/report.Rmd', params=list(CSV_DIR='$(DATA_DIR)/csv/_all', PRINT_DATA=TRUE))"

analyze-argument-promise-mode:
	mkdir -p $(DATA_DIR)/output/argument-promise-mode/table
	mkdir -p $(DATA_DIR)/output/argument-promise-mode/graph
	analysis/argument-promise-mode.R $(DATA_DIR) $(DATA_DIR)/output/argument-promise-mode/table $(DATA_DIR)/output/argument-promise-mode/graph

analyze-environment:
	mkdir -p $(DATA_DIR)/output/environment/table
	mkdir -p $(DATA_DIR)/output/environment/graph
	analysis/environment.R $(DATA_DIR) $(DATA_DIR)/output/environment/table $(DATA_DIR)/output/environment/graph

analyze-argument-position-laziness:
	mkdir -p $(DATA_DIR)/output/argument-position-laziness/table
	mkdir -p $(DATA_DIR)/output/argument-position-laziness/graph
	analysis/argument-position-laziness $(DATA_DIR) $(DATA_DIR)/output/argument-position-laziness/table $(DATA_DIR)/output/argument-position-laziness/graph

analyze-side-effects:
	mkdir -p $(DATA_DIR)/output/side-effects/table
	mkdir -p $(DATA_DIR)/output/side-effects/graph
	analysis/side-effects $(DATA_DIR) $(DATA_DIR)/output/side-effects/table $(DATA_DIR)/output/side-effects/graph

analyze-promise-memory-usage:
	mkdir -p $(DATA_DIR)/output/promise-memory-usage/table
	mkdir -p $(DATA_DIR)/output/promise-memory-usage/graph
	analysis/promise-memory-usage.R $(DATA_DIR) $(DATA_DIR)/output/promise-memory-usage/table $(DATA_DIR)/output/promise-memory-usage/graph

analyze-promise-lifespan:
	mkdir -p $(DATA_DIR)/output/promise-lifespan/table
	mkdir -p $(DATA_DIR)/output/promise-lifespan/graph
	analysis/promise-lifespan.R $(DATA_DIR) $(DATA_DIR)/output/promise-lifespan/table $(DATA_DIR)/output/promise-lifespan/graph

analyze: analyze-environment analyze-argument-promise-mode analyze-argument-position-laziness analyze-promise-memory-usage analyze-promise-lifespan

analysis-report:
	$(VANILLA_RSCRIPT) -e "rmarkdown::render('analysis/analysis.Rmd', params=list(analysis_output_dir='`readlink -f $(DATA_DIR)/output`'), knit_root_dir='$(shell pwd)')"
