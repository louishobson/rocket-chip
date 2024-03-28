EVAL_DIR := $(abspath ./)
BASE_DIR := $(abspath ../)

SUBDIR ?= $(shell date +"%Y%m%d-%H:%M:%S")
RESULTS_DIR := $(EVAL_DIR)/results/$(SUBDIR)

MILL := mill --no-server 
PROJECT := freechips.rocketchip.system

CONFIGS := \
	EntanglingIPrefetcherNoPrefetcherBaselineConfig \
	EntanglingIPrefetcherTableSize064Sets4WaysConfig \
	EntanglingIPrefetcherTableSize064Sets8WaysConfig \
	EntanglingIPrefetcherTableSize128Sets4WaysConfig \
	EntanglingIPrefetcherTableSize128Sets8WaysConfig \
	EntanglingIPrefetcherTableSize256Sets4WaysConfig \
	EntanglingIPrefetcherTableSize256Sets8WaysConfig \
	EntanglingIPrefetcherTableSize512Sets4WaysConfig \
	EntanglingIPrefetcherTableSize512Sets8WaysConfig \
	EntanglingIPrefetcherICacheSize064Sets2WaysNoPrefetcherConfig \
	EntanglingIPrefetcherICacheSize064Sets4WaysNoPrefetcherConfig \
	EntanglingIPrefetcherICacheSize128Sets2WaysNoPrefetcherConfig \
	EntanglingIPrefetcherICacheSize128Sets4WaysNoPrefetcherConfig \
	EntanglingIPrefetcherICacheSize256Sets2WaysNoPrefetcherConfig \
	EntanglingIPrefetcherICacheSize256Sets4WaysNoPrefetcherConfig \
	EntanglingIPrefetcherICacheSize064Sets2WaysWithPrefetcherConfig \
	EntanglingIPrefetcherICacheSize064Sets4WaysWithPrefetcherConfig \
	EntanglingIPrefetcherICacheSize128Sets2WaysWithPrefetcherConfig \
	EntanglingIPrefetcherICacheSize128Sets4WaysWithPrefetcherConfig \
	EntanglingIPrefetcherICacheSize256Sets2WaysWithPrefetcherConfig \
	EntanglingIPrefetcherICacheSize256Sets4WaysWithPrefetcherConfig \
	EntanglingIPrefetcherMaxEntanglings1Config \
	EntanglingIPrefetcherMaxEntanglings2Config \
	EntanglingIPrefetcherMaxEntanglings3Config \
	EntanglingIPrefetcherMaxEntanglings4Config \
	EntanglingIPrefetcherMaxEntanglings5Config \
	EntanglingIPrefetcherMaxEntanglings6Config 

ALL_BUILD_TARGETS = $(patsubst %,build-%,$(CONFIGS))
ALL_RUN_TARGETS = $(patsubst %,run-%,$(CONFIGS))

BUILD_CONFIG = $(patsubst build-%,%,$@)
RUN_CONFIG = $(patsubst run-%,%,$@)

build: $(ALL_BUILD_TARGETS)
build-%: 
	if [ ! -d $(BASE_DIR)/out/emulator/$(PROJECT).TestHarness/$(PROJECT).$(BUILD_CONFIG) ]; \
	then \
		cd $(BASE_DIR) && $(MILL) emulator[$(PROJECT).TestHarness,$(PROJECT).$(BUILD_CONFIG)].elf; \
	fi

run: $(ALL_RUN_TARGETS)
run-%::
	mkdir -p "$(RESULTS_DIR)/$(RUN_CONFIG)"

	if [ ! -f $(RESULTS_DIR)/$(RUN_CONFIG)/radiosity.output ]; \
	then \
	$(BASE_DIR)/out/emulator/$(PROJECT).TestHarness/$(PROJECT).$(RUN_CONFIG)/verilator/elf.dest/emulator \
		pk /home/ldh35/parsec_gc/ext/splash2x/apps/radiosity/inst/amd64-linux.gcc-serial/bin/radiosity \
			-bf 1.0e-1 -batch -p 1 \
		2>&1 >"$(RESULTS_DIR)/$(RUN_CONFIG)/radiosity.output" \
			| spike-dasm >"$(RESULTS_DIR)/$(RUN_CONFIG)/radiosity.log"; \
	fi

	if [ ! -f "$(RESULTS_DIR)/$(RUN_CONFIG)/barnes.output" ]; \
	then \
	$(BASE_DIR)/out/emulator/$(PROJECT).TestHarness/$(PROJECT).$(RUN_CONFIG)/verilator/elf.dest/emulator \
		pk /home/ldh35/parsec_gc/ext/splash2x/apps/barnes/inst/amd64-linux.gcc-serial/bin/barnes \
		2>&1 >"$(RESULTS_DIR)/$(RUN_CONFIG)/barnes.output" \
			| spike-dasm >"$(RESULTS_DIR)/$(RUN_CONFIG)/barnes.log"; \
	fi

	if [ ! -f "$(RESULTS_DIR)/$(RUN_CONFIG)/water_spatial.output" ]; \
	then \
	$(BASE_DIR)/out/emulator/$(PROJECT).TestHarness/$(PROJECT).$(RUN_CONFIG)/verilator/elf.dest/emulator \
		pk /home/ldh35/parsec_gc/ext/splash2x/apps/water_spatial/inst/amd64-linux.gcc-serial/bin/water_spatial \
			"$(shell cat /home/ldh35/parsec_gc/ext/splash2x/apps/water_spatial/run/input_1)" \
		2>&1 >"$(RESULTS_DIR)/$(RUN_CONFIG)/water_spatial.output" \
			| spike-dasm >"$(RESULTS_DIR)/$(RUN_CONFIG)/water_spatial.log"; \
	fi
	


	