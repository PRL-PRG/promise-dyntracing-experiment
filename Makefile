DATA_DIR=`date +'%y-%m-%d-%H-%M-%S'`
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
	if [ -e $(DATA_DIR)/csv/_all ]; then rm -r $(DATA_DIR)/csv/_all; fi
	graphs/aggregate_csvs.sh $(DATA_DIR)/csv/_all $(DATA_DIR)/csv/*
	graphs/generate_aggregated_data.sh $(DATA_DIR) _all
	#Rscript graphs/conglomerate_csvs.R $(DATA_DIR)/csv/_all

partial-aggregate-csvs:
	graphs/concat_functions.sh $(DATA_DIR)
	if [ -e $(DATA_DIR)/csv/_partial ]; then rm -r $(DATA_DIR)/csv/_partial; fi
	graphs/partial_aggregate_csvs.sh $(DATA_DIR)/csv/_partial $(DATA_DIR)/csv/*
	graphs/generate_aggregated_data.sh $(DATA_DIR) _partial
	#Rscript graphs/conglomerate_csvs.R $(DATA_DIR)/csv/_all


report:
	$(VANILLA_RSCRIPT) -e "rmarkdown::render('graphs/report-short.Rmd', params=list(CSV_DIR='$(DATA_DIR)/csv/_all', PRINT_DATA=TRUE))"
	mv graphs/report-short.html $(DATA_DIR)
	$(VANILLA_RSCRIPT) -e "rmarkdown::render('graphs/report.Rmd', params=list(CSV_DIR='$(DATA_DIR)/csv/_all', PRINT_DATA=TRUE))"
	mv graphs/report.html $(DATA_DIR)

analyze-argument-promise-mode:
	mkdir -p $(OUTPUT_DIR)/argument-promise-mode/table
	mkdir -p $(OUTPUT_DIR)/argument-promise-mode/graph
	mkdir -p $(OUTPUT_DIR)/argument-promise-mode/partial
	analysis/argument-promise-mode.R $(PART) $(DATA_DIR)/data $(OUTPUT_DIR)/argument-promise-mode/table $(OUTPUT_DIR)/argument-promise-mode/graph $(OUTPUT_DIR)/argument-promise-mode/partial $(OUTPUT_DIR)/argument-promise-mode/variables.sty

analyze-environment:
	mkdir -p $(OUTPUT_DIR)/environment/table
	mkdir -p $(OUTPUT_DIR)/environment/graph
	mkdir -p $(OUTPUT_DIR)/environment/partial
	analysis/environment.R $(PART) $(DATA_DIR)/data $(OUTPUT_DIR)/environment/table $(OUTPUT_DIR)/environment/graph $(OUTPUT_DIR)/environment/partial $(OUTPUT_DIR)/environment/variables.sty

analyze-position-evaluation-mode:
	mkdir -p $(OUTPUT_DIR)/position-evaluation-mode/table
	mkdir -p $(OUTPUT_DIR)/position-evaluation-mode/graph
	mkdir -p $(OUTPUT_DIR)/position-evaluation-mode/partial
	analysis/position-evaluation-mode.R $(PART) $(DATA_DIR)/data $(OUTPUT_DIR)/position-evaluation-mode/table $(OUTPUT_DIR)/position-evaluation-mode/graph $(OUTPUT_DIR)/position-evaluation-mode/partial $(OUTPUT_DIR)/position-evaluation-mode/variables.sty

analyze-side-effects:
	mkdir -p $(OUTPUT_DIR)/side-effects/table
	mkdir -p $(OUTPUT_DIR)/side-effects/graph
	mkdir -p $(OUTPUT_DIR)/side-effects/partial
	analysis/side-effects.R $(PART) $(DATA_DIR)/data $(OUTPUT_DIR)/side-effects/table $(OUTPUT_DIR)/side-effects/graph $(OUTPUT_DIR)/side-effects/partial $(OUTPUT_DIR)/side-effects/variables.sty

analyze-promise-memory-usage:
	mkdir -p $(OUTPUT_DIR)/promise-memory-usage/table
	mkdir -p $(OUTPUT_DIR)/promise-memory-usage/graph
	mkdir -p $(OUTPUT_DIR)/promise-memory-usage/partial
	analysis/promise-memory-usage.R $(PART) $(DATA_DIR)/data $(OUTPUT_DIR)/promise-memory-usage/table $(OUTPUT_DIR)/promise-memory-usage/graph $(OUTPUT_DIR)/promise-memory-usage/partial $(OUTPUT_DIR)/promise-memory-usage/variables.sty

analyze-promise-lifespan:
	mkdir -p $(OUTPUT_DIR)/promise-lifespan/table
	mkdir -p $(OUTPUT_DIR)/promise-lifespan/graph
	mkdir -p $(OUTPUT_DIR)/promise-lifespan/partial
	analysis/promise-lifespan.R $(PART) $(DATA_DIR)/data $(OUTPUT_DIR)/promise-lifespan/table $(OUTPUT_DIR)/promise-lifespan/graph $(OUTPUT_DIR)/promise-lifespan/partial $(OUTPUT_DIR)/promise-lifespan/variables.sty

analyze-function-force:
	mkdir -p $(OUTPUT_DIR)/function-force/table
	mkdir -p $(OUTPUT_DIR)/function-force/graph
	mkdir -p $(OUTPUT_DIR)/function-force/partial
	analysis/function-force.R $(PART) $(DATA_DIR)/data $(OUTPUT_DIR)/function-force/table $(OUTPUT_DIR)/function-force/graph $(OUTPUT_DIR)/function-force/partial $(OUTPUT_DIR)/function-force/variables.sty

analyze-general-info:
	mkdir -p $(OUTPUT_DIR)/general-info/table
	mkdir -p $(OUTPUT_DIR)/general-info/graph
	mkdir -p $(OUTPUT_DIR)/general-info/partial
	analysis/general-info.R $(PART) $(DATA_DIR)/data $(OUTPUT_DIR)/general-info/table $(OUTPUT_DIR)/general-info/graph $(OUTPUT_DIR)/general-info/partial $(OUTPUT_DIR)/general-info/variables.sty

analyze-jumps:
	mkdir -p $(OUTPUT_DIR)/jumps/table
	mkdir -p $(OUTPUT_DIR)/jumps/graph
	mkdir -p $(OUTPUT_DIR)/jumps/partial
	analysis/jumps.R $(PART) $(DATA_DIR)/data $(OUTPUT_DIR)/jumps/table $(OUTPUT_DIR)/jumps/graph $(OUTPUT_DIR)/jumps/partial $(OUTPUT_DIR)/jumps/variables.sty

analyze-accesses:
	mkdir -p $(OUTPUT_DIR)/accesses/table
	mkdir -p $(OUTPUT_DIR)/accesses/graph
	mkdir -p $(OUTPUT_DIR)/accesses/partial
	analysis/accesses.R $(PART) $(DATA_DIR)/data $(OUTPUT_DIR)/accesses/table $(OUTPUT_DIR)/accesses/graph $(OUTPUT_DIR)/accesses/partial $(OUTPUT_DIR)/accesses/variables.sty

analyze-function-returns:
	mkdir -p $(OUTPUT_DIR)/function-returns/table
	mkdir -p $(OUTPUT_DIR)/function-returns/graph
	mkdir -p $(OUTPUT_DIR)/function-returns/partial
	analysis/function-returns.R $(PART) $(DATA_DIR)/data $(OUTPUT_DIR)/function-returns/table $(OUTPUT_DIR)/function-returns/graph $(OUTPUT_DIR)/function-returns/partial $(OUTPUT_DIR)/function-returns/variables.sty

analyze-specific-calls:
	mkdir -p $(OUTPUT_DIR)/specific-calls/table
	mkdir -p $(OUTPUT_DIR)/specific-calls/graph
	mkdir -p $(OUTPUT_DIR)/specific-calls/partial
	analysis/specific-calls.R $(PART) $(DATA_DIR)/data $(OUTPUT_DIR)/specific-calls/table $(OUTPUT_DIR)/specific-calls/graph $(OUTPUT_DIR)/specific-calls/partial $(OUTPUT_DIR)/specific-calls/variables.sty

compute-interference:
	mkdir -p $(OUTPUT_DIR)/interference/table
	mkdir -p $(OUTPUT_DIR)/interference/graph
	analysis/interference.py $(DATA_DIR)/data $(OUTPUT_DIR)/interference/table

analyze-interference:
	mkdir -p $(OUTPUT_DIR)/interference/table
	mkdir -p $(OUTPUT_DIR)/interference/graph
	analysis/interference.R $(PART) $(DATA_DIR)/data $(OUTPUT_DIR)/interference/table $(OUTPUT_DIR)/interference/graph $(OUTPUT_DIR)/interference/partial $(OUTPUT_DIR)/interference/variables.sty

analyze: analyze-environment analyze-argument-promise-mode analyze-position-evaluation-mode analyze-promise-memory-usage analyze-promise-lifespan analyze-function-force

analysis-book:
	cd analysis/report; $(VANILLA_RSCRIPT) -e "bookdown::render_book(list.files('.'), 'bookdown::gitbook', output_dir='$(SITE_DIR)', config_file='_bookdown.yml', params=list(analysis_output_dir='`readlink -f $(OUTPUT_DIR)`'), knit_root_dir='$(shell pwd)')"

analysis-report:
	$(VANILLA_RSCRIPT) -e "rmarkdown::render('analysis/analysis.Rmd', params=list(analysis_output_dir='`readlink -f $(OUTPUT_DIR)`'), knit_root_dir='$(shell pwd)')"

install-dependencies:
	$(VANILLA_RSCRIPT) install-dependencies.Rmd

analyze-in-screens:
	screen -S analyze-promise-lifespan          -d -m bash -c "make analyze-promise-lifespan         DATA_DIR=$(DATA_DIR); read x"
	screen -S analyze-argument-promise-mode     -d -m bash -c "make analyze-argument-promise-mode    DATA_DIR=$(DATA_DIR); read x"
	screen -S analyze-interference              -d -m bash -c "make analyze-interference             DATA_DIR=$(DATA_DIR); read x"
	screen -S analyze-promise-memory-usage      -d -m bash -c "make analyze-promise-memory-usage     DATA_DIR=$(DATA_DIR); read x"
	screen -S analyze-environment               -d -m bash -c "make analyze-environment              DATA_DIR=$(DATA_DIR); read x"
	screen -S analyze-position-evaluation-mode  -d -m bash -c "make analyze-position-evaluation-mode DATA_DIR=$(DATA_DIR); read x"
	screen -S analyze-side-effects              -d -m bash -c "make analyze-side-effects             DATA_DIR=$(DATA_DIR); read x"
	screen -S compute-interference              -d -m bash -c "make compute-interference 		 DATA_DIR=$(DATA_DIR); read x"

paper-components:
	mkdir $(DATA_DIR)/paper-components
	cat $(OUTPUT_DIR)/*/variables.sty > $(DATA_DIR)/paper-components/variables.sty

tests:
	mkdir -p tests/data
	mkdir -p tests/output/table
	mkdir -p tests/output/graph

	export R_KEEP_PKG_SOURCE=yes; \
	export R_ENABLE_JIT=0; \
	export R_DISABLE_BYTECODE=1; \
	export R_COMPILE_PKGS=0; \
	cd tests/scripts/Shootout/fasta; \
	$(TRACER) --file=fasta.r; \
	$(TRACER) --file=fasta-alternative.r; \
	$(TRACER) --file=fasta-internal-rng.r;

	export R_KEEP_PKG_SOURCE=yes; \
	export R_ENABLE_JIT=0; \
	export R_DISABLE_BYTECODE=1; \
	export R_COMPILE_PKGS=0; \
	cd tests/scripts/Shootout/knucleotide; \
	$(TRACER) --file=k-nucleotide-alternative.r;

	export R_KEEP_PKG_SOURCE=yes; \
	export R_ENABLE_JIT=0; \
	export R_DISABLE_BYTECODE=1; \
	export R_COMPILE_PKGS=0; \
	cd tests/scripts/Shootout/spectralnorm; \
	$(TRACER) --file=spectral-norm.r;

	export R_KEEP_PKG_SOURCE=yes; \
	export R_ENABLE_JIT=0; \
	cd tests/scripts/Misc; \
	export R_DISABLE_BYTECODE=1; \
	export R_COMPILE_PKGS=0; \
	$(TRACER) --file=bin_packing.R; \
	$(TRACER) --file=io.R; \
	$(TRACER) --file=moving_average.R; \
	$(TRACER) --file=rgibbs.R;

	export R_KEEP_PKG_SOURCE=yes; \
	export R_ENABLE_JIT=0; \
	export R_DISABLE_BYTECODE=1; \
	export R_COMPILE_PKGS=0; \
	cd tests/scripts/MachineLearningAlg/MBO; \
	$(TRACER) --file=LibSVMMulticore.R;

	export R_KEEP_PKG_SOURCE=yes; \
	export R_ENABLE_JIT=0; \
	export R_DISABLE_BYTECODE=1; \
	export R_COMPILE_PKGS=0; \
	cd tests/scripts/MachineLearningAlg/learners-in-r; \
	$(TRACER) --file=naiveBayes.R; \
	$(TRACER) --file=qda.R;


	#$(TRACER) --file=knn.R;

	export R_KEEP_PKG_SOURCE=yes; \
	export R_ENABLE_JIT=0; \
	export R_DISABLE_BYTECODE=1; \
	export R_COMPILE_PKGS=0; \
	cd tests/scripts/MachineLearningAlg/main_functions; \
	$(TRACER)script ada.R 10 2; \
	$(TRACER)script cforest.R 11 2; \
	$(TRACER)script ctree.R 12 2; \
	$(TRACER)script glm_logistic.R 13 2; \
	$(TRACER)script glmnet_classification.R 14 2; \
	$(TRACER)script glmnet_regression.R 15 2; \
	$(TRACER)script knn.R 16 2; \
	$(TRACER)script ksvm.R 17 2; \
	$(TRACER)script lda.R 18 2; \
	$(TRACER)script lm.R 19 2; \
	$(TRACER)script mboost_classification.R 20 2; \
	$(TRACER)script mboost_regression.R 21 2; \
	$(TRACER)script pen_l1_classification.R 22 2; \
	$(TRACER)script pen_l1_regression.R 23 2; \
	$(TRACER)script problem_generation.R 24 2; \
	$(TRACER)script qda.R 25 2; \
	$(TRACER)script randomForest.R 26 2; \
	$(TRACER)script rda_as_lda.R 27 2; \
	$(TRACER)script rda_as_qda.R 28 2; \
	$(TRACER)script rda_regularized.R 29 2; \
	$(TRACER)script svm.R 30 2; \
	$(TRACER)script tree.R 31 2;

	find tests/scripts -name '*.sqlite' | xargs -I files mv -f files tests/data
	find tests/scripts -name '*.OK' | xargs -I files mv -f files tests/data
	find tests/scripts -name '*.trace' | xargs -I files mv -f files tests/data
	analysis/position-evaluation-mode.R all tests/data tests/output/table tests/output/graph

.PHONY: tests
