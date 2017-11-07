INPUT_DIR=2017-11-06-13-41-34
OUTPUT_DIR=output

analyze-argument-promise-mode:
	mkdir -p $(OUTPUT_DIR)/argument-promise-mode/table
	mkdir -p $(OUTPUT_DIR)/argument-promise-mode/graph
	analysis/argument-promise-mode $(INPUT_DIR) $(OUTPUT_DIR)/argument-promise-mode/table $(OUTPUT_DIR)/argument-promise-mode/graph

analyze-environment:
	mkdir -p $(OUTPUT_DIR)/environment/table
	mkdir -p $(OUTPUT_DIR)/environment/graph
	analysis/environment $(INPUT_DIR) $(OUTPUT_DIR)/environment/table $(OUTPUT_DIR)/environment/graph

analyze: analyze-environment analyze-argument-promise-mode
