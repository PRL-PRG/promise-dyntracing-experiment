################################################################################
## R-Dyntrace
################################################################################
R_DYNTRACE_DIRPATH := ../R-dyntrace
R_DYNTRACE := $(R_DYNTRACE_DIRPATH)/bin/R
R_DYNTRACE_FLAGS := --slave --no-restore --no-save

################################################################################
## tee
################################################################################
TEE := tee
TEE_FLAGS := --ignore-interrupts
################################################################################
## xvfb
################################################################################
XVFB_RUN := xvfb-run

################################################################################
## time
################################################################################
TIME := time

################################################################################
## tracer output directory paths
################################################################################
TRACE_DIRPATH := $(shell date +'%Y-%m-%d-%H-%M-%S')
LATEST_TRACE_DIRPATH := $(shell readlink -f latest)
TRACE_ANALYSIS_DIRPATH := $(TRACE_DIRPATH)/analysis
TRACE_ANALYSIS_RAW_DIRPATH := $(TRACE_ANALYSIS_DIRPATH)/raw
TRACE_ANALYSIS_REDUCED_DIRPATH := $(TRACE_ANALYSIS_DIRPATH)/reduced
TRACE_ANALYSIS_COMBINED_DIRPATH := $(TRACE_ANALYSIS_DIRPATH)/combined
TRACE_ANALYSIS_SUMMARIZED_DIRPATH := $(TRACE_ANALYSIS_DIRPATH)/summarized
TRACE_ANALYSIS_VISUALIZED_DIRPATH := $(TRACE_ANALYSIS_DIRPATH)/visualized
TRACE_ANALYSIS_REPORT_DIRPATH := $(TRACE_ANALYSIS_DIRPATH)/report
TRACE_ANALYSIS_REPORT_FILEPATH := $(TRACE_ANALYSIS_REPORT_DIRPATH)/report.html
TRACE_ANALYSIS_TRACES_DIRPATH := $(ANALYSIS_DIRPATH)/traces
TRACE_CORPUS_DIRPATH := $(TRACE_DIRPATH)/corpus
TRACE_LOGS_DIRPATH := $(TRACE_DIRPATH)/logs
TRACE_LOGS_RAW_DIRPATH := $(TRACE_LOGS_DIRPATH)/raw
TRACE_LOGS_REDUCED_DIRPATH := $(TRACE_LOGS_DIRPATH)/reduced
TRACE_LOGS_COMBINED_DIRPATH := $(TRACE_LOGS_DIRPATH)/combined
TRACE_LOGS_SUMMARIZED_DIRPATH := $(TRACE_LOGS_DIRPATH)/summarized
TRACE_LOGS_VISUALIZED_DIRPATH := $(TRACE_LOGS_DIRPATH)/visualized
TRACE_LOGS_REPORT_DIRPATH := $(TRACE_LOGS_DIRPATH)/report
TRACE_LOGS_CORPUS_DIRPATH := $(TRACE_LOGS_DIRPATH)/corpus
TRACE_LOGS_SUMMARY_DIRPATH := $(TRACE_LOGS_DIRPATH)/summary
TRACE_LOGS_SUMMARY_RAW_DIRPATH := $(TRACE_LOGS_SUMMARY_DIRPATH)/raw
TRACE_LOGS_SUMMARY_REDUCED_DIRPATH := $(TRACE_LOGS_SUMMARY_DIRPATH)/reduced
TRACE_LOGS_SUMMARY_COMBINED_FILEPATH := $(TRACE_LOGS_SUMMARY_DIRPATH)/combined
TRACE_LOGS_SUMMARY_SUMMARIZED_FILEPATH := $(TRACE_LOGS_SUMMARY_DIRPATH)/summarized
TRACE_LOGS_SUMMARY_VISUALIZED_FILEPATH := $(TRACE_LOGS_SUMMARY_DIRPATH)/visualized
TRACE_LOGS_SUMMARY_REPORT_FILEPATH := $(TRACE_LOGS_SUMMARY_DIRPATH)/report

################################################################################
## report directory paths
################################################################################
REPORT_TEMPLATE_DIRPATH := report
REPORT_TEMPLATE_FILEPATH := $(REPORT_TEMPLATE_DIRPATH)/report.Rmd

################################################################################
## package setup options
################################################################################
CRAN_MIRROR_URL := "https://cran.r-project.org"
PACKAGE_SETUP_REPOSITORIES := --setup-cran --setup-bioc
PACKAGE_SETUP_NCPUS := 8
PACKAGE_SETUP_DIRPATH := ~
CRAN_LIB_DIRPATH := $(PACKAGE_SETUP_DIRPATH)/CRAN/lib
CRAN_SRC_DIRPATH := $(PACKAGE_SETUP_DIRPATH)/CRAN/src
CRAN_LOG_DIRPATH := $(PACKAGE_SETUP_DIRPATH)/CRAN/log
BIOC_LIB_DIRPATH := $(PACKAGE_SETUP_DIRPATH)/BIOC/lib
BIOC_SRC_DIRPATH := $(PACKAGE_SETUP_DIRPATH)/BIOC/src
BIOC_LOG_DIRPATH := $(PACKAGE_SETUP_DIRPATH)/BIOC/log

################################################################################
## GNU Parallel arguments
################################################################################
PARALLEL_JOB_COUNT := 1
PARALLEL_JOB_COUNT_FILEPATH := scripts/procfile
TRACE_TRACING_SCRIPT_FILEPATH := scripts/trace.R
CORPUS_FILEPATH := scripts/corpus.txt
DEPENDENCIES_FILEPATH := scripts/dependencies.txt

################################################################################
## tracer arguments
################################################################################
BINARY := --binary
COMPRESSION_LEVEL := 0
TRUNCATE := --truncate
## timeout value in seconds
TRACING_TIMEOUT := 3600
ENABLE_TRACE :=
## to enable verbose mode, use the flag: --verbose
VERBOSE :=

TRACE_ANALYSIS_SCRIPT_TYPE := --vignettes --examples --tests
################################################################################
## analysis arguments
################################################################################
SCHEMA_DIR := schemas
STAGE := all
USE_CACHE := --use-cache
ANALYSIS := function-strictness

################################################################################
## data table viewer arguments
################################################################################
DATA_TABLE_VIEWER_SCRIPT := scripts/view-data-table.R
DATA_TABLE_VIEWER_FILEPATH :=
DATA_TABLE_VIEWER_ARGS :=

################################################################################
## lint arguments
################################################################################
LINT_FILEPATH := scripts/create-corpus-with-matching-patterns.R

export R_KEEP_PKG_SOURCE=1
export R_ENABLE_JIT=0
export R_COMPILE_PKGS=0
export R_DISABLE_BYTECODE=1


define tracer =
$(TIME) $(R_DYNTRACE) --slave                                                     \
                      --no-restore                                                \
                      --file=$(TRACE_TRACING_SCRIPT_FILEPATH)                     \
                      --args --r-dyntrace=$(R_DYNTRACE)                           \
                             --tracing-timeout=$(TRACING_TIMEOUT)                 \
                             --corpus-dirpath=$(TRACE_CORPUS_DIRPATH)             \
                             --raw-analysis-dirpath=$(TRACE_ANALYSIS_RAW_DIRPATH) \
                             --trace-dirpath=$(TRACE_ANALYSIS_TRACES_DIRPATH)     \
                             $(ENABLE_TRACE)	                                    \
                             $(BINARY)                                            \
                             --compression-level=$(COMPRESSION_LEVEL)             \
                             $(ANALYSIS_SWITCH)                                   \
                             $(VERBOSE)                                           \
                             $(TRUNCATE)
endef


define parallel =
parallel --jobs $(PARALLEL_JOB_COUNT_FILEPATH)      \
         --files                                    \
         --bar                                      \
         --load 80%                                 \
         --results $(TRACE_LOGS_RAW_DIRPATH)/{1}/   \
         --joblog $(TRACE_LOGS_SUMMARY_RAW_DIRPATH) \
           $(tracer)                                \
           {1}
endef


define trace =
	@echo "R_ENABLE_JIT=${R_ENABLE_JIT}"
	@echo "R_COMPILE_PKGS=${R_COMPILE_PKGS}"
	@echo "PARALLEL JOB COUNT=${PARALLEL_JOB_COUNT}"
	@echo $(PARALLEL_JOB_COUNT) > $(PARALLEL_JOB_COUNT_FILEPATH)
	@mkdir -p $(TRACE_LOGS_DIRPATH)
	@mkdir -p $(TRACE_LOGS_RAW_DIRPATH)
	@mkdir -p $(TRACE_LOGS_SUMMARY_DIRPATH)

	@if [ -e $(LATEST_TRACE_DIRPATH) ]; then \
		unlink latest; \
	fi
	@ln -fs $(TRACE_DIRPATH) latest
	$(parallel) :::: $(CORPUS_FILEPATH) > /dev/null
endef


trace-jit: R_ENABLE_JIT=3
trace-jit: R_COMPILE_PKGS=1
trace-jit: R_DISABLE_BYTECODE=0
trace-jit: R_KEEP_PKG_SOURCE=1
trace-jit:
	$(trace)


trace-ast: R_ENABLE_JIT=0
trace-ast: R_COMPILE_PKGS=0
trace-ast: R_DISABLE_BYTECODE=1
trace-ast: R_KEEP_PKG_SOURCE=1
trace-ast:
	$(trace)


install-dependencies: R_ENABLE_JIT=0
install-dependencies: R_COMPILE_PKGS=0
install-dependencies: R_DISABLE_BYTECODE=1
install-dependencies: R_KEEP_PKG_SOURCE=1
install-dependencies:
	$(R_DYNTRACE) --file=scripts/install-packages.R --args $(DEPENDENCIES_FILEPATH)


install-corpus: R_ENABLE_JIT=0
install-corpus: R_COMPILE_PKGS=0
install-corpus: R_DISABLE_BYTECODE=1
install-corpus: R_KEEP_PKG_SOURCE=1
install-corpus:
	$(R_DYNTRACE) $(R_DYNTRACE_FLAGS)               \
	              --file=scripts/install-packages.R \
	              --args $(CORPUS_FILEPATH)


corpus:
	@echo "Updating vignette list in '$(CORPUS_FILEPATH)'"
	$(R_DYNTRACE) $(R_DYNTRACE_FLAGS)                \
	              --file=scripts/make-package-list.R \
	              --args $(CORPUS_FILEPATH)


analyze:
	@mkdir -p $(INPUT_DIR)/output/$(ANALYSIS)/logs/

	@$(R_DYNTRACE) $(R_DYNTRACE_FLAGS)                            \
	               --file=analysis/$(ANALYSIS).R                  \
	               --args                                         \
	               --stage=$(STAGE)                               \
	               $(SCHEMA_DIR)                                  \
	               $(INPUT_DIR)/analysis/raw                      \
	               $(INPUT_DIR)/output/$(ANALYSIS)/summary        \
	               $(INPUT_DIR)/output/$(ANALYSIS)/visualizations \
	               $(INPUT_DIR)/output/$(ANALYSIS)/latex
#2>&1 | tee $(INPUT_DIR)/output/$(ANALYSIS)/log.txt || /bin/true


view-data-table:
	@$(R_DYNTRACE) $(R_DYNTRACE_FLAGS)                  \
	               --file=$(DATA_TABLE_VIEWER_SCRIPT)   \
	               --args $(DATA_TABLE_VIEWER_ARGS)     \
	                      $(DATA_TABLE_VIEWER_FILEPATH)


lint:
	@$(R_DYNTRACE) $(R_DYNTRACE_FLAGS)                  \
	               -e "lintr::lint('$(LINT_FILEPATH)')"


add-dependents-and-dependencies:
	@(TIME) $(R_DYNTRACE) --slave                                                         \
	                      --file=scripts/add-dependents-and-dependencies.R                \
	                      --args scripts/delayed-assign-force-force-and-call-packages.csv \
	                      scripts/corpus-paper.csv                                        \
	                      --dependents                                                    \
	                      --dependencies


setup-package-repositories:
	@mkdir -p $(CRAN_LOG_DIRPATH)
	@mkdir -p $(BIOC_LOG_DIRPATH)
	@$(TIME) $(R_DYNTRACE) $(R_DYNTRACE_FLAGS)                           \
	                       --file=scripts/setup-package-repositories.R   \
	                       --args $(PACKAGE_SETUP_REPOSITORIES)          \
	                              $(PACKAGE_SETUP_NCPUS)                 \
	                              --cran-mirror-url=$(CRAN_MIRROR_URL)   \
	                              --cran-lib-dirpath=$(CRAN_LIB_DIRPATH) \
	                              --cran-src-dirpath=$(CRAN_SRC_DIRPATH) \
	                              --cran-log-dirpath=$(CRAN_LOG_DIRPATH) \
	                              --bioc-lib-dirpath=$(BIOC_LIB_DIRPATH) \
	                              --bioc-src-dirpath=$(BIOC_SRC_DIRPATH) \
	                              --bioc-log-dirpath=$(BIOC_LOG_DIRPATH) \


reduce-analysis:
	@mkdir -p $(TRACE_LOGS_SUMMARY_DIRPATH)
	@mkdir -p $(TRACE_LOGS_REDUCED_DIRPATH)

	-@$(TIME) parallel --jobs $(PARALLEL_JOB_COUNT)                               \
	                   --files                                                    \
	                   --bar                                                      \
	                   --results $(TRACE_LOGS_REDUCED_DIRPATH)/{1}                \
	                   --joblog $(TRACE_LOGS_SUMMARY_REDUCED_DIRPATH)             \
	                   $(R_DYNTRACE) $(R_DYNTRACE_FLAGS)                          \
	                                 --file=analysis/$(ANALYSIS)/reduce.R         \
	                                 --args $(TRACE_ANALYSIS_RAW_DIRPATH)/{1}     \
	                                        $(TRACE_ANALYSIS_REDUCED_DIRPATH)/{1} \
	                                        $(TRACE_ANALYSIS_SCRIPT_TYPE)         \
	                   "2>&1"                                                     \
	                   ::: $(shell cd $(TRACE_ANALYSIS_RAW_DIRPATH) && ls -d */) > /dev/null


combine-analysis:
	@mkdir -p $(TRACE_LOGS_SUMMARY_DIRPATH)
	@mkdir -p $(TRACE_LOGS_COMBINED_DIRPATH)

	@$(TIME) $(R_DYNTRACE) $(R_DYNTRACE_FLAGS)                              \
	                       --file=analysis/$(ANALYSIS)/combine.R            \
	                       --args $(TRACE_ANALYSIS_REDUCED_DIRPATH)         \
	                              $(TRACE_ANALYSIS_COMBINED_DIRPATH)        \
	                              $(TRACE_ANALYSIS_SCRIPT_TYPE)             \
	                       2>&1 | $(TEE) $(TEE_FLAGS)                       \
	                                     $(TRACE_LOGS_COMBINED_DIRPATH)/log


summarize-analysis:
	@mkdir -p $(TRACE_LOGS_SUMMARY_DIRPATH)
	@mkdir -p $(TRACE_LOGS_SUMMARIZED_DIRPATH)

	@$(TIME) $(R_DYNTRACE) $(R_DYNTRACE_FLAGS)                                \
	                       --file=analysis/$(ANALYSIS)/summarize.R            \
	                       --args $(TRACE_ANALYSIS_COMBINED_DIRPATH)          \
	                              $(TRACE_ANALYSIS_SUMMARIZED_DIRPATH)        \
	                       2>&1 | $(TEE) $(TEE_FLAGS)                         \
	                                     $(TRACE_LOGS_SUMMARIZED_DIRPATH)/log


visualize-analysis:
	@mkdir -p $(TRACE_LOGS_SUMMARY_DIRPATH)
	@mkdir -p $(TRACE_LOGS_VISUALIZED_DIRPATH)

	@$(TIME) $(R_DYNTRACE) $(R_DYNTRACE_FLAGS)                                \
	                       --file=analysis/$(ANALYSIS)/visualize.R            \
	                       --args $(TRACE_ANALYSIS_SUMMARIZED_DIRPATH)        \
	                              $(TRACE_ANALYSIS_VISUALIZED_DIRPATH)        \
	                       2>&1 | $(TEE) $(TEE_FLAGS)                         \
	                                     $(TRACE_LOGS_VISUALIZED_DIRPATH)/log


report-analysis:
	@mkdir -p $(TRACE_LOGS_SUMMARY_DIRPATH)
	@mkdir -p $(TRACE_LOGS_REPORT_DIRPATH)

	@$(TIME) $(XVFB_RUN) $(R_DYNTRACE) $(R_DYNTRACE_FLAGS)                            \
	                                   --file=analysis/$(ANALYSIS)/report.R           \
	                                   --args $(REPORT_TEMPLATE_FILEPATH)             \
	                                          $(TRACE_ANALYSIS_REPORT_FILEPATH)       \
	                                          $(TRACE_ANALYSIS_SUMMARIZED_DIRPATH)    \
	                                          $(TRACE_ANALYSIS_VISUALIZED_DIRPATH)    \
	                                   2>&1 | $(TEE) $(TEE_FLAGS)                     \
	                                                 $(TRACE_LOGS_REPORT_DIRPATH)/log


analyze-corpus:
	@mkdir -p $(TRACE_LOGS_SUMMARY_DIRPATH)
	@mkdir -p $(TRACE_LOGS_CORPUS_DIRPATH)

	@$(TIME) $(R_DYNTRACE) $(R_DYNTRACE_FLAGS)                              \
	  	                    --file=analysis/corpus.R                        \
	    	                  --args $(TRACE_CORPUS_DIRPATH)                  \
	      	                       $(TRACE_ANALYSIS_RAW_DIRPATH)            \
	        	                     $(TRACE_ANALYSIS_CORPUS_DIRPATH)         \
	          	            2>&1 | $(TEE) $(TEE_FLAGS)                      \
	            	                        $(TRACE_LOGS_CORPUS_DIRPATH)/log


.PHONY: trace                           \
	      corpus                          \
	      install-dependencies            \
	      analyze                         \
	      clean                           \
	      view-data-table                 \
	      report                          \
	      lint                            \
	      add-dependents-and-dependencies \
	      reduce-analysis                 \
	      combine-analysis                \
	      summarize-analysis              \
	      visualize-analysis              \
	      report-analysis                 \
	      analyze-corpus
