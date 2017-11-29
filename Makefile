INPUT_DIR=2017-11-06-13-41-34
OUTPUT_DIR=output

TRACER=../better-R-dyntrace/bin/R


trace: 
	dyntrace/packages.sh $(TRACER) $(INPUT_DIR) $(PACKAGES)

analyze-argument-promise-mode:
	mkdir -p $(OUTPUT_DIR)/argument-promise-mode/table
	mkdir -p $(OUTPUT_DIR)/argument-promise-mode/graph
	analysis/argument-promise-mode.R $(INPUT_DIR) $(OUTPUT_DIR)/argument-promise-mode/table $(OUTPUT_DIR)/argument-promise-mode/graph

analyze-environment:
	mkdir -p $(OUTPUT_DIR)/environment/table
	mkdir -p $(OUTPUT_DIR)/environment/graph
	analysis/environment.R $(INPUT_DIR) $(OUTPUT_DIR)/environment/table $(OUTPUT_DIR)/environment/graph

analyze: analyze-environment analyze-argument-promise-mode


	


