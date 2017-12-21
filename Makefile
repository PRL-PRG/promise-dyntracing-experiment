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

analyze-argument-position-laziness:
	mkdir -p $(OUTPUT_DIR)/argument-position-laziness/table
	mkdir -p $(OUTPUT_DIR)/argument-position-laziness/graph
	analysis/argument-position-laziness $(INPUT_DIR) $(OUTPUT_DIR)/argument-position-laziness/table $(OUTPUT_DIR)/argument-position-laziness/graph

analyze-side-effects:
	mkdir -p $(OUTPUT_DIR)/side-effects/table
	mkdir -p $(OUTPUT_DIR)/side-effects/graph
	analysis/side-effects $(INPUT_DIR) $(OUTPUT_DIR)/side-effects/table $(OUTPUT_DIR)/side-effects/graph

analyze: analyze-environment analyze-argument-promise-mode analyze-argument-position-laziness
