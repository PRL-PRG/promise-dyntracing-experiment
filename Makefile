################################################################################
## R-Dyntrace location
################################################################################
R_DYNTRACE_DIRPATH := ../R-dyntrace
R_DYNTRACE := $(R_DYNTRACE_DIRPATH)/bin/R

################################################################################
## tracer output directory paths
################################################################################
TRACE_DIRPATH := $(shell date +'%Y-%m-%d-%H-%M-%S')
LATEST_TRACE_DIRPATH := $(shell readlink -f latest)
TRACE_ANALYSIS_DIRPATH := $(TRACE_DIRPATH)/analysis
TRACE_ANALYSIS_RAW_DIRPATH := $(TRACE_ANALYSIS_DIRPATH)/raw
TRACE_ANALYSIS_REDUCED_DIRPATH := $(TRACE_ANALYSIS_DIRPATH)/reduced
TRACE_ANALYSIS_TRACES_DIRPATH := $(ANALYSIS_DIRPATH)/traces
TRACE_CORPUS_DIRPATH := $(TRACE_DIRPATH)/corpus
TRACE_LOGS_DIRPATH := $(TRACE_DIRPATH)/logs
TRACE_LOGS_RAW_DIRPATH := $(TRACE_LOGS_DIRPATH)/raw
TRACE_LOGS_REDUCE_DIRPATH := $(TRACE_LOGS_DIRPATH)/reduce
TRACE_LOGS_SUMMARY_DIRPATH := $(TRACE_LOGS_DIRPATH)/summary
TRACE_LOGS_SUMMARY_RAW_DIRPATH := $(TRACE_LOGS_SUMMARY_DIRPATH)/raw
TRACE_LOGS_SUMMARY_REDUCE_DIRPATH := $(TRACE_LOGS_SUMMARY_DIRPATH)/reduced

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
## By default all analyses are enabled.
## To disable a particular analysis, do --disable-<analysis-name>-analysis
## Ex: ANALYSIS_SWITCH=--disable-metadata-analysis\ --disable-strictness-analysis
## Note the \ which escapes the next space and makes multiple flags part of same
## variable
ANALYSIS_SWITCH :=
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
## report arguments
################################################################################
REPORT_DIRPATH := report
BROWSER := chromium

################################################################################
## lint arguments
################################################################################
LINT_FILEPATH := scripts/create-corpus-with-matching-patterns.R

export R_KEEP_PKG_SOURCE=1
export R_ENABLE_JIT=0
export R_COMPILE_PKGS=0
export R_DISABLE_BYTECODE=1

define tracer =
$(R_DYNTRACE) \
    --slave \
    --no-restore \
    --file=$(TRACE_TRACING_SCRIPT_FILEPATH) \
    --args \
    --r-dyntrace=$(R_DYNTRACE) \
	  --tracing-timeout=$(TRACING_TIMEOUT) \
    --corpus-dirpath=$(TRACE_CORPUS_DIRPATH) \
    --raw-analysis-dirpath=$(TRACE_ANALYSIS_RAW_DIRPATH) \
    --trace-dirpath=$(TRACE_ANALYSIS_TRACES_DIRPATH) \
    $(ENABLE_TRACE)	\
    $(BINARY) \
    --compression-level=$(COMPRESSION_LEVEL) \
    $(ANALYSIS_SWITCH) \
    $(VERBOSE) \
    $(TRUNCATE)
endef

define parallel =
parallel \
    --jobs $(PARALLEL_JOB_COUNT_FILEPATH) \
    --files \
    --bar \
    --load 80% \
    --results $(TRACE_LOGS_RAW_DIRPATH)/{1}/ \
    --joblog $(TRACE_LOGS_SUMMARY_RAW_DIRPATH) \
      $(tracer) \
      {1}
endef

define count_files =
$(shell find $(LATEST_TRACE_DIRPATH)/analysis/raw/ -mindepth 3 -maxdepth 3 -type f -name "$(1)" | wc -l)
endef

define count_dirs =
$(shell find $(LATEST_TRACE_DIRPATH)/analysis/raw/ -mindepth $(1) -maxdepth $(1) -type d | wc -l)
endef

define dir_size =
$(shell du -sh $(LATEST_TRACE_DIRPATH)/$(1) | cut -f1)
endef

define infer_pid =
$(shell ps axf | grep $(1) | grep -v "grep" | awk '{print $$1}')
endef

define infer_child_processes =
$(shell ps -eo ppid= | grep -Fwc $(1))
endef

define statistics_table =
	PACKAGES    VIGNETTES \
	$(call count_dirs,1)    $(call count_dirs,2) \n\
	ANALYSIS    LOGS    CORPUS \n\
	$(call dir_size,analysis/raw/)    $(call dir_size,logs)    $(call dir_size,corpus)\n\
	BEGIN    ACTIVE    NOERROR    ERROR\n\
	$(call count_files,BEGIN)  $(call infer_child_processes, $(call infer_pid,'perl.*parallel')) \
  $(call count_files,NOERROR)    $(call count_files,ERROR)
endef


define trace =
	@echo "R_ENABLE_JIT=${R_ENABLE_JIT}"
	@echo "R_COMPILE_PKGS=${R_COMPILE_PKGS}"
	@echo "PARALLEL JOB COUNT=${PARALLEL_JOB_COUNT}"
	@echo $(PARALLEL_JOB_COUNT) > $(PARALLEL_JOB_COUNT_FILEPATH)
	@mkdir -p $(TRACE_LOGS_DIRPATH)
	@mkdir -p $(TRACE_LOGS_SUMMARY_DIRPATH)
	@mkdir -p $(TRACE_LOGS_REDUCE_DIRPATH)

	@if [ -e $(LATEST_TRACE_DIRPATH) ]; then \
		unlink latest; \
	fi
	@ln -fs $(TRACE_DIRPATH) latest
	-$(parallel) :::: $(CORPUS_FILEPATH) > /dev/null
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
	$(R_DYNTRACE) --file=scripts/install-packages.R --args $(CORPUS_FILEPATH)

statistics:
	@echo "$(call statistics_table, $(LATEST_TRACE_DIRPATH))" | column -t

corpus:
	@echo "Updating vignette list in '$(CORPUS_FILEPATH)'"
	$(R_DYNTRACE) --file=scripts/make-package-list.R --args $(CORPUS_FILEPATH)

analyze:
	mkdir -p $(INPUT_DIR)/output/$(ANALYSIS)/logs/
	$(R_DYNTRACE) --slave --file=analysis/$(ANALYSIS).R --args --stage=$(STAGE) $(SCHEMA_DIR) $(INPUT_DIR)/analysis/raw $(INPUT_DIR)/output/$(ANALYSIS)/summary $(INPUT_DIR)/output/$(ANALYSIS)/visualizations $(INPUT_DIR)/output/$(ANALYSIS)/latex
#2>&1 | tee $(INPUT_DIR)/output/$(ANALYSIS)/log.txt || /bin/true

view-data-table:
	$(R_DYNTRACE) --slave --file=$(DATA_TABLE_VIEWER_SCRIPT) --args $(DATA_TABLE_VIEWER_ARGS) $(DATA_TABLE_VIEWER_FILEPATH)

report:
	$(R_DYNTRACE) --slave -e "rmarkdown::render('$(REPORT_DIRPATH)/report.Rmd', output_file='report.html', runtime = 'auto', params = list(output_directory='$(LATEST_TRACE_DIRPATH)/output'))"
# output_dir='$(REPORT_DIRPATH)/', intermediates_dir='$(REPORT_DIRPATH)/', knit_root_dir='$(REPORT_DIRPATH)/'

lint:
	$(R_DYNTRACE) --slave -e "lintr::lint('$(LINT_FILEPATH)')"

add-dependents-and-dependencies:
	$(R_DYNTRACE) --slave --file=scripts/add-dependents-and-dependencies.R --args scripts/delayed-assign-force-force-and-call-packages.csv scripts/corpus-paper.csv --dependents --dependencies

.PHONY: trace statistics corpus install-dependencies analyze clean view-data-table report lint add-dependents-and-dependencies

reduce-analysis:
	@mkdir -p $(TRACE_LOGS_SUMMARY_DIRPATH)
	@mkdir -p $(TRACE_LOGS_REDUCE_DIRPATH)

	-parallel --jobs $(PARALLEL_JOB_COUNT) \
	         --files \
	         --bar \
	         --load 80% \
	         --results $(TRACE_LOGS_REDUCE_DIRPATH)/{1}/ \
	         --joblog $(TRACE_LOGS_SUMMARY_REDUCE_DIRPATH) \
	         $(R_DYNTRACE) \
	         --slave \
	         --no-restore \
	         --file=analysis/$(ANALYSIS)/reduce.R \
	         --args \
	         $(TRACE_ANALYSIS_RAW_DIRPATH)/{1} \
	         $(TRACE_ANALYSIS_REDUCED_DIRPATH)/{1} \
	         $(TRACE_ANALYSIS_SCRIPT_TYPE) \
	         ::: $(shell cd $(TRACE_ANALYSIS_RAW_DIRPATH) && ls -d */) > /dev/null
