DATA_DIR=`date +'%y-%m-%d-%H-%M-%S'`
LOG_FILE=log-`date +'%y-%m-%d-%H-%M-%S'`.txt
OUTPUT_DIR=$(DATA_DIR)/output
TRACER=../R-dyntrace/bin/R
PROCESSES=1
PACKAGES=
MINIMUM_DISK_SIZE=50000000 # ~50GB
STAGE=all
USE_CACHE=--use-cache
VANILLA_R=R
VANILLA_RSCRIPT=Rscript
SITE_DIR=~/public_html
ANALYSIS=promise-memory-usage
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
	$(VANILLA_RSCRIPT) -e "rmarkdown::render('graphs/report-main.Rmd', params=list(CSV_DIR='$(DATA_DIR)/csv/_partial', PRINT_DATA=TRUE))"
	mv graphs/report-main.html $(DATA_DIR)
#	$(VANILLA_RSCRIPT) -e "rmarkdown::render('graphs/report-short.Rmd', params=list(CSV_DIR='$(DATA_DIR)/csv/_all', PRINT_DATA=TRUE))"
#	mv graphs/report-short.html $(DATA_DIR)
#	$(VANILLA_RSCRIPT) -e "rmarkdown::render('graphs/report.Rmd', params=list(CSV_DIR='$(DATA_DIR)/csv/_all', PRINT_DATA=TRUE))"
#	mv graphs/report.html $(DATA_DIR)

analyze:
	analysis/$(ANALYSIS).R $(USE_CACHE) --stage=$(STAGE) $(DATA_DIR)/data $(OUTPUT_DIR)/$(ANALYSIS)/summary $(OUTPUT_DIR)/$(ANALYSIS)/visualizations $(OUTPUT_DIR)/$(ANALYSIS)/variables.sty $(OUTPUT_DIR)/$(ANALYSIS)/cache 2>&1 | tee $(OUTPUT_DIR)/$(ANALYSIS)/$(LOG_FILE)

analysis-book:
	cd analysis/report; $(VANILLA_RSCRIPT) -e "bookdown::render_book(list.files('.'), 'bookdown::gitbook', output_dir='$(SITE_DIR)', config_file='_bookdown.yml', params=list(analysis_output_dir='`readlink -f $(OUTPUT_DIR)`'), knit_root_dir='$(shell pwd)')"

analysis-report:
	$(VANILLA_RSCRIPT) -e "rmarkdown::render('analysis/analysis.Rmd', params=list(analysis_output_dir='`readlink -f $(OUTPUT_DIR)`'), knit_root_dir='$(shell pwd)')"

install-dependencies:
	$(VANILLA_RSCRIPT) install-dependencies.Rmd

analyze-in-screens:
	screen -S analyze-promise-lifespan          -d -m bash -c "make analyze ANALYSIS=promise-lifespan         DATA_DIR=$(DATA_DIR); read x"
	screen -S analyze-argument-promise-mode     -d -m bash -c "make analyze ANALYSIS=argument-promise-mode    DATA_DIR=$(DATA_DIR); read x"
	screen -S analyze-promise-memory-usage      -d -m bash -c "make analyze ANALYSIS=promise-memory-usage     DATA_DIR=$(DATA_DIR); read x"
	screen -S analyze-environment               -d -m bash -c "make analyze ANALYSIS=environment              DATA_DIR=$(DATA_DIR); read x"
	screen -S analyze-position-evaluation-mode  -d -m bash -c "make analyze ANALYSIS=position-evaluation-mode DATA_DIR=$(DATA_DIR); read x"
	screen -S analyze-side-effects              -d -m bash -c "make analyze ANALYSIS=side-effects             DATA_DIR=$(DATA_DIR); read x"
	screen -S analyze-specific-calls            -d -m bash -c "make analyze ANALYSIS=specific-calls 		      DATA_DIR=$(DATA_DIR); read x"
	screen -S analyze-jumps                     -d -m bash -c "make analyze ANALYSIS=jumps 		                DATA_DIR=$(DATA_DIR); read x"
	screen -S analyze-accesses                  -d -m bash -c "make analyze ANALYSIS=accesses 		            DATA_DIR=$(DATA_DIR); read x"
	screen -S analyze-function-returns          -d -m bash -c "make analyze ANALYSIS=function-returns 		    DATA_DIR=$(DATA_DIR); read x"
	screen -S analyze-general-info              -d -m bash -c "make analyze ANALYSIS=general-info 		        DATA_DIR=$(DATA_DIR); read x"
	#screen -S compute-interference              -d -m bash -c "make analyze ANALYSIS=compute-interference 		         DATA_DIR=$(DATA_DIR); read x"
	#screen -S analyze-interference              -d -m bash -c "make analyze ANALYSIS=interference             DATA_DIR=$(DATA_DIR); read x"

paper-components:
	mkdir -p $(DATA_DIR)/paper-components
	mkdir -p $(DATA_DIR)/paper-components/figures
	cat $(OUTPUT_DIR)/*/variables.sty > $(DATA_DIR)/paper-components/variables.sty
	cp $(OUTPUT_DIR)/*/visualizations/*.pdf $(DATA_DIR)/paper-components/figures/

compute-interference:
	mkdir -p $(OUTPUT_DIR)/interference/summary
	mkdir -p $(OUTPUT_DIR)/interference/visualizations
	analysis/interference.py $(DATA_DIR)/data $(OUTPUT_DIR)/interference/table

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
