DATA_DIR=data/2017-11-06-13-41-34
OUTPUT_DIR=$(DATA_DIR)/output
TRACER=../R-dyntrace/bin/R
PROCESSES=1
PACKAGES=
MINIMUM_DISK_SIZE=50000000 # ~50GB
PART=all
VANILLA_R=R
VANILLA_RSCRIPT=Rscript
SITE_DIR=~/public_html
trace:
	dyntrace/packages.sh $(TRACER) $(DATA_DIR) $(PROCESSES) $(MINIMUM_DISK_SIZE) $(PACKAGES)

check:
	dyntrace/check_results.sh $(DATA_DIR)

get-todo-packages:
	dyntrace/get_todo_list.sh $(DATA_DIR)

clean:
	dyntrace/clean_results.sh $(DATA_DIR)

csvs:
	graphs/generate_data.sh $(PROCESSES) $(DATA_DIR)

aggregate-csvs:
	graphs/concat_functions.sh $(DATA_DIR)
	if [ -e $(DATA_DIR)/csv/_all ]; then rm -ri $(DATA_DIR)/csv/_all; fi
	graphs/aggregate_csvs.sh $(DATA_DIR)/csv/_all $(DATA_DIR)/csv/*
	graphs/generate_aggregated_data.sh $(DATA_DIR)
	Rscript graphs/conglomerate_csvs.R $(DATA_DIR)/csv/_all

report:
	$(VANILLA_RSCRIPT) -e "rmarkdown::render('graphs/report.Rmd', params=list(CSV_DIR='$(DATA_DIR)/csv/_all', PRINT_DATA=TRUE))"
	mv graphs/report.html $(DATA_DIR)

analyze-argument-promise-mode:
	mkdir -p $(OUTPUT_DIR)/argument-promise-mode/table
	mkdir -p $(OUTPUT_DIR)/argument-promise-mode/graph
	analysis/argument-promise-mode.R $(DATA_DIR)/data $(OUTPUT_DIR)/argument-promise-mode/table $(OUTPUT_DIR)/argument-promise-mode/graph

analyze-environment:
	mkdir -p $(OUTPUT_DIR)/environment/table
	mkdir -p $(OUTPUT_DIR)/environment/graph
	analysis/environment.R $(PART) $(DATA_DIR)/data $(OUTPUT_DIR)/environment/table $(OUTPUT_DIR)/environment/graph

analyze-argument-position-laziness:
	mkdir -p $(OUTPUT_DIR)/argument-position-laziness/table
	mkdir -p $(OUTPUT_DIR)/argument-position-laziness/graph
	analysis/argument-position-laziness $(DATA_DIR)/data $(OUTPUT_DIR)/argument-position-laziness/table $(OUTPUT_DIR)/argument-position-laziness/graph

analyze-side-effects:
	mkdir -p $(OUTPUT_DIR)/side-effects/table
	mkdir -p $(OUTPUT_DIR)/side-effects/graph
	analysis/side-effects $(DATA_DIR)/data $(OUTPUT_DIR)/side-effects/table $(OUTPUT_DIR)/side-effects/graph

analyze-promise-memory-usage:
	mkdir -p $(OUTPUT_DIR)/promise-memory-usage/table
	mkdir -p $(OUTPUT_DIR)/promise-memory-usage/graph
	analysis/promise-memory-usage.R $(PART) $(DATA_DIR)/data $(OUTPUT_DIR)/promise-memory-usage/table $(OUTPUT_DIR)/promise-memory-usage/graph

analyze-promise-lifespan:
	mkdir -p $(OUTPUT_DIR)/promise-lifespan/table
	mkdir -p $(OUTPUT_DIR)/promise-lifespan/graph
	analysis/promise-lifespan.R $(PART) $(DATA_DIR)/data $(OUTPUT_DIR)/promise-lifespan/table $(OUTPUT_DIR)/promise-lifespan/graph

analyze: analyze-environment analyze-argument-promise-mode analyze-argument-position-laziness analyze-promise-memory-usage analyze-promise-lifespan

analysis-book:
	cd analysis/report; $(VANILLA_RSCRIPT) -e "bookdown::render_book('_bookdown.yml', 'bookdown::gitbook', output_dir='$(SITE_DIR)', params=list(analysis_output_dir='`readlink -f ../../$(OUTPUT_DIR)`'), knit_root_dir='$(shell pwd)')"

analysis-report:
	$(VANILLA_RSCRIPT) -e "rmarkdown::render('analysis/analysis.Rmd', params=list(analysis_output_dir='`readlink -f $(OUTPUT_DIR)`'), knit_root_dir='$(shell pwd)')"
