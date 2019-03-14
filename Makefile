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
TIME := time --portability

################################################################################
## unbuffer
################################################################################
UNBUFFER := unbuffer

################################################################################
## tracer output directory paths
################################################################################
TRACE_DIRPATH := $(shell date +'%Y-%m-%d-%H-%M-%S')
LATEST_TRACE_DIRPATH := $(shell readlink -f latest)
TRACE_ANALYSIS_DIRPATH := $(TRACE_DIRPATH)/analysis
TRACE_ANALYSIS_RAW_DIRPATH := $(TRACE_ANALYSIS_DIRPATH)/raw
TRACE_ANALYSIS_CORPUS_DIRPATH := $(TRACE_ANALYSIS_DIRPATH)/corpus
TRACE_ANALYSIS_REDUCED_DIRPATH := $(TRACE_ANALYSIS_DIRPATH)/reduced
TRACE_ANALYSIS_SCANNED_DIRPATH := $(TRACE_ANALYSIS_DIRPATH)/scanned
TRACE_ANALYSIS_COMBINED_DIRPATH := $(TRACE_ANALYSIS_DIRPATH)/combined
TRACE_ANALYSIS_MERGED_DIRPATH := $(TRACE_ANALYSIS_DIRPATH)/merged
TRACE_ANALYSIS_SUMMARIZED_DIRPATH := $(TRACE_ANALYSIS_DIRPATH)/summarized
TRACE_ANALYSIS_VISUALIZED_DIRPATH := $(TRACE_ANALYSIS_DIRPATH)/visualized
TRACE_ANALYSIS_REPORT_DIRPATH := $(TRACE_ANALYSIS_DIRPATH)/report
TRACE_CORPUS_DIRPATH := $(TRACE_DIRPATH)/corpus
TRACE_LOGS_DIRPATH := $(TRACE_DIRPATH)/logs
TRACE_LOGS_RAW_DIRPATH := $(TRACE_LOGS_DIRPATH)/raw
TRACE_LOGS_REDUCED_DIRPATH := $(TRACE_LOGS_DIRPATH)/reduced
TRACE_LOGS_SCANNED_DIRPATH := $(TRACE_LOGS_DIRPATH)/scanned
TRACE_LOGS_COMBINED_DIRPATH := $(TRACE_LOGS_DIRPATH)/combined
TRACE_LOGS_MERGED_DIRPATH := $(TRACE_LOGS_DIRPATH)/merged
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
## combine variables
################################################################################
COMBINE_COUNT := 10
COMBINED_FILENAME_PREFIX = $(shell hostname)-part

################################################################################
## scan variables
################################################################################
ALL_SCRIPTS_FILEPATH := "all_scripts.csv"
VALID_SCRIPTS_FILEPATH := "valid_scripts.csv"
INVALID_SCRIPTS_FILEPATH := "invalid_scripts.csv"

################################################################################
## report directory paths
################################################################################
REPORT_TEMPLATE_DIRPATH := report
REPORT_UTILITIES_SCRIPTPATH := report/utilities.R

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
## to enable verbose mode, use the flag: --verbose
VERBOSE :=

TRACE_ANALYSIS_SCRIPT_TYPE := --vignettes --examples --tests
################################################################################
## analysis arguments
################################################################################
ANALYSIS := object_types

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
                      --args --tracing-timeout=$(TRACING_TIMEOUT)                 \
	                           --r-dyntrace=$(R_DYNTRACE)                           \
                             --corpus-dirpath=$(TRACE_CORPUS_DIRPATH)             \
                             --raw-analysis-dirpath=$(TRACE_ANALYSIS_RAW_DIRPATH) \
                             $(VERBOSE)                                           \
                             $(TRUNCATE)                                          \
                             $(BINARY)                                            \
                             --compression-level=$(COMPRESSION_LEVEL)
endef


define parallel =
parallel --jobs $(PARALLEL_JOB_COUNT_FILEPATH)      \
         --files                                    \
         --bar                                      \
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
	@$(R_DYNTRACE) $(R_DYNTRACE_FLAGS)             \
	               --file=scripts/create-corpus.R  \
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
	@mkdir -p $(TRACE_LOGS_SUMMARY_REDUCED_DIRPATH)
	@mkdir -p $(TRACE_LOGS_REDUCED_DIRPATH)/$(ANALYSIS)
	@mkdir -p $(TRACE_ANALYSIS_REDUCED_DIRPATH)/$(ANALYSIS)

	-@$(TIME) parallel --jobs $(PARALLEL_JOB_COUNT)                                            \
	                   --files                                                                 \
	                   --bar                                                                   \
	                   --results $(TRACE_LOGS_REDUCED_DIRPATH)/$(ANALYSIS)/{1}                 \
	                   --joblog $(TRACE_LOGS_SUMMARY_REDUCED_DIRPATH)/$(ANALYSIS)              \
	                   $(R_DYNTRACE) $(R_DYNTRACE_FLAGS)                                       \
	                                 --file=analysis/parameters/reduce.R                       \
	                                 --args $(TRACE_ANALYSIS_RAW_DIRPATH)/{1}                  \
	                                        $(TRACE_ANALYSIS_REDUCED_DIRPATH)/$(ANALYSIS)/{1}  \
	                                        $(ANALYSIS)                                        \
	                                        $(TRACE_ANALYSIS_SCRIPT_TYPE)                      \
	                                        $(BINARY)                                          \
	                                        --compression-level=$(COMPRESSION_LEVEL)           \
	                   "2>&1"                                                                  \
	                   ::: $(shell find $(TRACE_ANALYSIS_RAW_DIRPATH) -mindepth 3 -maxdepth 3 -type d -printf "%P\n") > /dev/null


scan-analyses:
	@mkdir -p $(TRACE_LOGS_SCANNED_DIRPATH)
	@mkdir -p $(TRACE_ANALYSIS_SCANNED_DIRPATH)

	@$(UNBUFFER) $(TIME) $(R_DYNTRACE) $(R_DYNTRACE_FLAGS)                                                  \
	                                   --file=analysis/parameters/scan.R                                    \
	                                   --args $(TRACE_ANALYSIS_REDUCED_DIRPATH)                             \
	                                          $(TRACE_ANALYSIS_SCANNED_DIRPATH)/$(ALL_SCRIPTS_FILEPATH)     \
	                                          $(TRACE_ANALYSIS_SCANNED_DIRPATH)/$(VALID_SCRIPTS_FILEPATH)   \
	                                          $(TRACE_ANALYSIS_SCANNED_DIRPATH)/$(INVALID_SCRIPTS_FILEPATH) \
	                                          $(TRACE_ANALYSIS_SCRIPT_TYPE)                                 \
	                                          2>&1 | $(TEE) $(TEE_FLAGS)                                    \
	                                                 $(TRACE_LOGS_SCANNED_DIRPATH)/log


combine-analysis:
	@mkdir -p $(TRACE_LOGS_SUMMARY_DIRPATH)
	@mkdir -p $(TRACE_LOGS_COMBINED_DIRPATH)

	@$(UNBUFFER) $(TIME) $(R_DYNTRACE) $(R_DYNTRACE_FLAGS)                                                  \
	                                   --file=analysis/parameters/combine.R                                 \
	                                   --args $(TRACE_ANALYSIS_REDUCED_DIRPATH)                             \
	                                          $(TRACE_ANALYSIS_COMBINED_DIRPATH)                            \
	                                          $(TRACE_ANALYSIS_SCANNED_DIRPATH)/$(VALID_SCRIPTS_FILEPATH)   \
	                                          $(ANALYSIS)                                                   \
	                                          $(COMBINE_COUNT)                                              \
	                                          $(TRACE_ANALYSIS_SCRIPT_TYPE)                                 \
	                                          $(BINARY)                                                     \
	                                          --compression-level=$(COMPRESSION_LEVEL)                      \
	                                          --combined-filename-prefix=$(COMBINED_FILENAME_PREFIX)        \
	                                          2>&1 | $(TEE) $(TEE_FLAGS)                                    \
	                                                 $(TRACE_LOGS_COMBINED_DIRPATH)/$(ANALYSIS)


merge-analysis:
	@mkdir -p $(TRACE_LOGS_SUMMARY_DIRPATH)
	@mkdir -p $(TRACE_LOGS_MERGED_DIRPATH)

	@$(UNBUFFER) $(TIME) $(R_DYNTRACE) $(R_DYNTRACE_FLAGS)                                           \
	                                   --file=analysis/parameters/merge.R                            \
	                                   --args $(TRACE_ANALYSIS_COMBINED_DIRPATH)                     \
	                                          $(TRACE_ANALYSIS_MERGED_DIRPATH)                       \
	                                          $(ANALYSIS)                                            \
	                                          $(BINARY)                                              \
	                                          --compression-level=$(COMPRESSION_LEVEL)               \
	                                          2>&1 | $(TEE) $(TEE_FLAGS)                             \
	                                                 $(TRACE_LOGS_MERGED_DIRPATH)/$(ANALYSIS)


summarize-analysis:
	@mkdir -p $(TRACE_LOGS_SUMMARY_DIRPATH)
	@mkdir -p $(TRACE_LOGS_SUMMARIZED_DIRPATH)

	@$(UNBUFFER) $(TIME) $(R_DYNTRACE) $(R_DYNTRACE_FLAGS)                                           \
	                                   --file=analysis/parameters/summarize.R                        \
	                                   --args $(TRACE_ANALYSIS_MERGED_DIRPATH)                       \
	                                          $(TRACE_ANALYSIS_SUMMARIZED_DIRPATH)                   \
	                                          $(ANALYSIS)                                            \
	                                          $(BINARY)                                              \
	                                          --compression-level=$(COMPRESSION_LEVEL)               \
	                                   2>&1 | $(TEE) $(TEE_FLAGS)                                    \
	                                                 $(TRACE_LOGS_SUMMARIZED_DIRPATH)/$(ANALYSIS)


visualize-analysis:
	@mkdir -p $(TRACE_LOGS_SUMMARY_DIRPATH)
	@mkdir -p $(TRACE_LOGS_VISUALIZED_DIRPATH)

	@$(UNBUFFER) $(TIME) $(R_DYNTRACE) $(R_DYNTRACE_FLAGS)                                        \
	                                   --file=analysis/parameters/visualize.R                     \
	                                   --args $(TRACE_ANALYSIS_SUMMARIZED_DIRPATH)                \
	                                          $(TRACE_ANALYSIS_VISUALIZED_DIRPATH)                \
	                                          $(ANALYSIS)                                         \
	                                          $(BINARY)                                           \
	                                          --compression-level=$(COMPRESSION_LEVEL)            \
	                                   2>&1 | $(TEE) $(TEE_FLAGS)                                 \
	                                                 $(TRACE_LOGS_VISUALIZED_DIRPATH)/$(ANALYSIS)


report-analysis:
	@mkdir -p $(TRACE_LOGS_SUMMARY_DIRPATH)
	@mkdir -p $(TRACE_LOGS_REPORT_DIRPATH)

	@$(UNBUFFER) $(TIME) $(XVFB_RUN) $(R_DYNTRACE) $(R_DYNTRACE_FLAGS)                                      \
	                                               --file=analysis/parameters/report.R                      \
	                                               --args $(REPORT_TEMPLATE_DIRPATH)/$(ANALYSIS).Rmd        \
	                                                      $(TRACE_ANALYSIS_REPORT_DIRPATH)/$(ANALYSIS).html \
	                                                      $(TRACE_ANALYSIS_SUMMARIZED_DIRPATH)              \
	                                                      $(TRACE_ANALYSIS_VISUALIZED_DIRPATH)              \
	                                                      $(BINARY)                                         \
	                                                  --compression-level=$(COMPRESSION_LEVEL)              \
	                                               2>&1 | $(TEE) $(TEE_FLAGS)                               \
	                                                             $(TRACE_LOGS_REPORT_DIRPATH)/$(ANALYSIS)


analyze-corpus:
	@mkdir -p $(TRACE_LOGS_SUMMARY_DIRPATH)
	@mkdir -p $(TRACE_LOGS_CORPUS_DIRPATH)
	@mkdir -p $(TRACE_ANALYSIS_CORPUS_DIRPATH)

	@$(UNBUFFER) $(TIME) $(R_DYNTRACE) $(R_DYNTRACE_FLAGS)                             \
	  	                               --file=analysis/corpus.R                        \
	    	                             --args $(TRACE_CORPUS_DIRPATH)                  \
	      	                                  $(TRACE_ANALYSIS_RAW_DIRPATH)            \
	        	                                $(TRACE_ANALYSIS_CORPUS_DIRPATH)         \
	          	                       2>&1 | $(TEE) $(TEE_FLAGS)                      \
	            	                                $(TRACE_LOGS_CORPUS_DIRPATH)/log


reduce-analyses:
	$(MAKE) reduce-analysis TRACE_DIRPATH=$(TRACE_DIRPATH) PARALLEL_JOB_COUNT=$(PARALLEL_JOB_COUNT) BINARY=$(BINARY) COMPRESSION_LEVEL=$(COMPRESSION_LEVEL) ANALYSIS=objects
	$(MAKE) reduce-analysis TRACE_DIRPATH=$(TRACE_DIRPATH) PARALLEL_JOB_COUNT=$(PARALLEL_JOB_COUNT) BINARY=$(BINARY) COMPRESSION_LEVEL=$(COMPRESSION_LEVEL) ANALYSIS=escaped_arguments
	$(MAKE) reduce-analysis TRACE_DIRPATH=$(TRACE_DIRPATH) PARALLEL_JOB_COUNT=$(PARALLEL_JOB_COUNT) BINARY=$(BINARY) COMPRESSION_LEVEL=$(COMPRESSION_LEVEL) ANALYSIS=function_definitions
	$(MAKE) reduce-analysis TRACE_DIRPATH=$(TRACE_DIRPATH) PARALLEL_JOB_COUNT=$(PARALLEL_JOB_COUNT) BINARY=$(BINARY) COMPRESSION_LEVEL=$(COMPRESSION_LEVEL) ANALYSIS=functions
	$(MAKE) reduce-analysis TRACE_DIRPATH=$(TRACE_DIRPATH) PARALLEL_JOB_COUNT=$(PARALLEL_JOB_COUNT) BINARY=$(BINARY) COMPRESSION_LEVEL=$(COMPRESSION_LEVEL) ANALYSIS=promises
	$(MAKE) reduce-analysis TRACE_DIRPATH=$(TRACE_DIRPATH) PARALLEL_JOB_COUNT=$(PARALLEL_JOB_COUNT) BINARY=$(BINARY) COMPRESSION_LEVEL=$(COMPRESSION_LEVEL) ANALYSIS=arguments
	$(MAKE) reduce-analysis TRACE_DIRPATH=$(TRACE_DIRPATH) PARALLEL_JOB_COUNT=$(PARALLEL_JOB_COUNT) BINARY=$(BINARY) COMPRESSION_LEVEL=$(COMPRESSION_LEVEL) ANALYSIS=parameters


reduce-analyses-prl-server:
	$(MAKE) reduce-analysis TRACE_DIRPATH=$(TRACE_DIRPATH) PARALLEL_JOB_COUNT=70 BINARY=$(BINARY) COMPRESSION_LEVEL=$(COMPRESSION_LEVEL) ANALYSIS=objects
	$(MAKE) reduce-analysis TRACE_DIRPATH=$(TRACE_DIRPATH) PARALLEL_JOB_COUNT=70 BINARY=$(BINARY) COMPRESSION_LEVEL=$(COMPRESSION_LEVEL) ANALYSIS=escaped_arguments
	$(MAKE) reduce-analysis TRACE_DIRPATH=$(TRACE_DIRPATH) PARALLEL_JOB_COUNT=70 BINARY=$(BINARY) COMPRESSION_LEVEL=$(COMPRESSION_LEVEL) ANALYSIS=function_definitions
	$(MAKE) reduce-analysis TRACE_DIRPATH=$(TRACE_DIRPATH) PARALLEL_JOB_COUNT=30 BINARY=$(BINARY) COMPRESSION_LEVEL=$(COMPRESSION_LEVEL) ANALYSIS=functions
	$(MAKE) reduce-analysis TRACE_DIRPATH=$(TRACE_DIRPATH) PARALLEL_JOB_COUNT=30 BINARY=$(BINARY) COMPRESSION_LEVEL=$(COMPRESSION_LEVEL) ANALYSIS=promises
	$(MAKE) reduce-analysis TRACE_DIRPATH=$(TRACE_DIRPATH) PARALLEL_JOB_COUNT=30 BINARY=$(BINARY) COMPRESSION_LEVEL=$(COMPRESSION_LEVEL) ANALYSIS=arguments
	$(MAKE) reduce-analysis TRACE_DIRPATH=$(TRACE_DIRPATH) PARALLEL_JOB_COUNT=30 BINARY=$(BINARY) COMPRESSION_LEVEL=$(COMPRESSION_LEVEL) ANALYSIS=parameters


combine-analyses:
	$(MAKE) combine-analysis TRACE_DIRPATH=$(TRACE_DIRPATH) BINARY=$(BINARY) COMPRESSION_LEVEL=$(COMPRESSION_LEVEL) COMBINE_COUNT=$(COMBINE_COUNT) ANALYSIS=objects
	$(MAKE) combine-analysis TRACE_DIRPATH=$(TRACE_DIRPATH) BINARY=$(BINARY) COMPRESSION_LEVEL=$(COMPRESSION_LEVEL) COMBINE_COUNT=$(COMBINE_COUNT) ANALYSIS=escaped_arguments
	$(MAKE) combine-analysis TRACE_DIRPATH=$(TRACE_DIRPATH) BINARY=$(BINARY) COMPRESSION_LEVEL=$(COMPRESSION_LEVEL) COMBINE_COUNT=$(COMBINE_COUNT) ANALYSIS=function_definitions
	$(MAKE) combine-analysis TRACE_DIRPATH=$(TRACE_DIRPATH) BINARY=$(BINARY) COMPRESSION_LEVEL=$(COMPRESSION_LEVEL) COMBINE_COUNT=$(COMBINE_COUNT) ANALYSIS=functions
	$(MAKE) combine-analysis TRACE_DIRPATH=$(TRACE_DIRPATH) BINARY=$(BINARY) COMPRESSION_LEVEL=$(COMPRESSION_LEVEL) COMBINE_COUNT=$(COMBINE_COUNT) ANALYSIS=promises
	$(MAKE) combine-analysis TRACE_DIRPATH=$(TRACE_DIRPATH) BINARY=$(BINARY) COMPRESSION_LEVEL=$(COMPRESSION_LEVEL) COMBINE_COUNT=$(COMBINE_COUNT) ANALYSIS=arguments
	$(MAKE) combine-analysis TRACE_DIRPATH=$(TRACE_DIRPATH) BINARY=$(BINARY) COMPRESSION_LEVEL=$(COMPRESSION_LEVEL) COMBINE_COUNT=$(COMBINE_COUNT) ANALYSIS=parameters


merge-analyses:
	$(MAKE) merge-analysis TRACE_DIRPATH=$(TRACE_DIRPATH) BINARY=$(BINARY) COMPRESSION_LEVEL=$(COMPRESSION_LEVEL) ANALYSIS=objects
	$(MAKE) merge-analysis TRACE_DIRPATH=$(TRACE_DIRPATH) BINARY=$(BINARY) COMPRESSION_LEVEL=$(COMPRESSION_LEVEL) ANALYSIS=escaped_arguments
	$(MAKE) merge-analysis TRACE_DIRPATH=$(TRACE_DIRPATH) BINARY=$(BINARY) COMPRESSION_LEVEL=$(COMPRESSION_LEVEL) ANALYSIS=function_definitions
	$(MAKE) merge-analysis TRACE_DIRPATH=$(TRACE_DIRPATH) BINARY=$(BINARY) COMPRESSION_LEVEL=$(COMPRESSION_LEVEL) ANALYSIS=functions
	$(MAKE) merge-analysis TRACE_DIRPATH=$(TRACE_DIRPATH) BINARY=$(BINARY) COMPRESSION_LEVEL=$(COMPRESSION_LEVEL) ANALYSIS=promises
	$(MAKE) merge-analysis TRACE_DIRPATH=$(TRACE_DIRPATH) BINARY=$(BINARY) COMPRESSION_LEVEL=$(COMPRESSION_LEVEL) ANALYSIS=arguments
	$(MAKE) merge-analysis TRACE_DIRPATH=$(TRACE_DIRPATH) BINARY=$(BINARY) COMPRESSION_LEVEL=$(COMPRESSION_LEVEL) ANALYSIS=parameters


summarize-analyses:
	$(MAKE) summarize-analysis TRACE_DIRPATH=$(TRACE_DIRPATH) BINARY=$(BINARY) COMPRESSION_LEVEL=$(COMPRESSION_LEVEL) ANALYSIS=objects
	$(MAKE) summarize-analysis TRACE_DIRPATH=$(TRACE_DIRPATH) BINARY=$(BINARY) COMPRESSION_LEVEL=$(COMPRESSION_LEVEL) ANALYSIS=escaped_arguments
	$(MAKE) summarize-analysis TRACE_DIRPATH=$(TRACE_DIRPATH) BINARY=$(BINARY) COMPRESSION_LEVEL=$(COMPRESSION_LEVEL) ANALYSIS=function_definitions
	$(MAKE) summarize-analysis TRACE_DIRPATH=$(TRACE_DIRPATH) BINARY=$(BINARY) COMPRESSION_LEVEL=$(COMPRESSION_LEVEL) ANALYSIS=functions
	$(MAKE) summarize-analysis TRACE_DIRPATH=$(TRACE_DIRPATH) BINARY=$(BINARY) COMPRESSION_LEVEL=$(COMPRESSION_LEVEL) ANALYSIS=promises
	$(MAKE) summarize-analysis TRACE_DIRPATH=$(TRACE_DIRPATH) BINARY=$(BINARY) COMPRESSION_LEVEL=$(COMPRESSION_LEVEL) ANALYSIS=arguments
	$(MAKE) summarize-analysis TRACE_DIRPATH=$(TRACE_DIRPATH) BINARY=$(BINARY) COMPRESSION_LEVEL=$(COMPRESSION_LEVEL) ANALYSIS=parameters


visualize-analyses:
	$(MAKE) visualize-analysis TRACE_DIRPATH=$(TRACE_DIRPATH) BINARY=$(BINARY) COMPRESSION_LEVEL=$(COMPRESSION_LEVEL) ANALYSIS=objects
	$(MAKE) visualize-analysis TRACE_DIRPATH=$(TRACE_DIRPATH) BINARY=$(BINARY) COMPRESSION_LEVEL=$(COMPRESSION_LEVEL) ANALYSIS=escaped_arguments
	$(MAKE) visualize-analysis TRACE_DIRPATH=$(TRACE_DIRPATH) BINARY=$(BINARY) COMPRESSION_LEVEL=$(COMPRESSION_LEVEL) ANALYSIS=functions
	$(MAKE) visualize-analysis TRACE_DIRPATH=$(TRACE_DIRPATH) BINARY=$(BINARY) COMPRESSION_LEVEL=$(COMPRESSION_LEVEL) ANALYSIS=promises
	$(MAKE) visualize-analysis TRACE_DIRPATH=$(TRACE_DIRPATH) BINARY=$(BINARY) COMPRESSION_LEVEL=$(COMPRESSION_LEVEL) ANALYSIS=arguments
	$(MAKE) visualize-analysis TRACE_DIRPATH=$(TRACE_DIRPATH) BINARY=$(BINARY) COMPRESSION_LEVEL=$(COMPRESSION_LEVEL) ANALYSIS=parameters


report-analyses:
	$(MAKE) report-analysis TRACE_DIRPATH=$(TRACE_DIRPATH) BINARY=$(BINARY) COMPRESSION_LEVEL=$(COMPRESSION_LEVEL) ANALYSIS=objects
	$(MAKE) report-analysis TRACE_DIRPATH=$(TRACE_DIRPATH) BINARY=$(BINARY) COMPRESSION_LEVEL=$(COMPRESSION_LEVEL) ANALYSIS=escaped_arguments
	$(MAKE) report-analysis TRACE_DIRPATH=$(TRACE_DIRPATH) BINARY=$(BINARY) COMPRESSION_LEVEL=$(COMPRESSION_LEVEL) ANALYSIS=functions
	$(MAKE) report-analysis TRACE_DIRPATH=$(TRACE_DIRPATH) BINARY=$(BINARY) COMPRESSION_LEVEL=$(COMPRESSION_LEVEL) ANALYSIS=promises
	$(MAKE) report-analysis TRACE_DIRPATH=$(TRACE_DIRPATH) BINARY=$(BINARY) COMPRESSION_LEVEL=$(COMPRESSION_LEVEL) ANALYSIS=arguments
	$(MAKE) report-analysis TRACE_DIRPATH=$(TRACE_DIRPATH) BINARY=$(BINARY) COMPRESSION_LEVEL=$(COMPRESSION_LEVEL) ANALYSIS=parameters


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
	      merge-analysis                  \
	      summarize-analysis              \
	      visualize-analysis              \
	      report-analysis                 \
	      analyze-corpus                  \
	      reduce-analyses                 \
	      reduce-analyses-prl-server      \
	      combine-analyses                \
	      merge-analyses                  \
	      summarize-analyses              \
	      visualize-analyses              \
	      report-analyses
