EVAL_DIR := $(abspath ./)
BASE_DIR := $(abspath ../)

SUBDIR ?= $(shell date +"%Y%m%d-%H:%M:%S")
RESULTS_DIR := $(EVAL_DIR)/results/$(SUBDIR)

MILL := mill --no-server 
PROJECT := freechips.rocketchip.system

TABLESIZE_CONFIGS := \
	EntanglingIPrefetcherTableSize002Sets2WaysConfig \
	EntanglingIPrefetcherTableSize002Sets4WaysConfig \
	EntanglingIPrefetcherTableSize002Sets8WaysConfig \
	EntanglingIPrefetcherTableSize004Sets2WaysConfig \
	EntanglingIPrefetcherTableSize004Sets4WaysConfig \
	EntanglingIPrefetcherTableSize004Sets8WaysConfig \
	EntanglingIPrefetcherTableSize008Sets2WaysConfig \
	EntanglingIPrefetcherTableSize008Sets4WaysConfig \
	EntanglingIPrefetcherTableSize008Sets8WaysConfig \
	EntanglingIPrefetcherTableSize016Sets2WaysConfig \
	EntanglingIPrefetcherTableSize016Sets4WaysConfig \
	EntanglingIPrefetcherTableSize016Sets8WaysConfig \
	EntanglingIPrefetcherTableSize032Sets2WaysConfig \
	EntanglingIPrefetcherTableSize032Sets4WaysConfig \
	EntanglingIPrefetcherTableSize032Sets8WaysConfig \
	EntanglingIPrefetcherTableSize064Sets2WaysConfig \
	EntanglingIPrefetcherTableSize064Sets4WaysConfig \
	EntanglingIPrefetcherTableSize064Sets8WaysConfig \
	EntanglingIPrefetcherTableSize128Sets2WaysConfig \
	EntanglingIPrefetcherTableSize128Sets4WaysConfig \
	EntanglingIPrefetcherTableSize128Sets8WaysConfig \
	EntanglingIPrefetcherTableSize256Sets2WaysConfig \
	EntanglingIPrefetcherTableSize256Sets4WaysConfig \
	EntanglingIPrefetcherTableSize256Sets8WaysConfig \
	EntanglingIPrefetcherTableSize512Sets2WaysConfig \
	EntanglingIPrefetcherTableSize512Sets4WaysConfig \
	EntanglingIPrefetcherTableSize512Sets8WaysConfig

CACHESIZE_CONFIGS := \
	EntanglingIPrefetcherICacheSize064Sets2WaysNoPrefetcherConfig \
	EntanglingIPrefetcherICacheSize064Sets4WaysNoPrefetcherConfig \
	EntanglingIPrefetcherICacheSize128Sets2WaysNoPrefetcherConfig \
	EntanglingIPrefetcherICacheSize128Sets4WaysNoPrefetcherConfig \
	EntanglingIPrefetcherICacheSize256Sets2WaysNoPrefetcherConfig \
	EntanglingIPrefetcherICacheSize256Sets4WaysNoPrefetcherConfig \
	EntanglingIPrefetcherICacheSize512Sets2WaysNoPrefetcherConfig \
	EntanglingIPrefetcherICacheSize512Sets4WaysNoPrefetcherConfig \
	EntanglingIPrefetcherICacheSize064Sets2WaysWithPrefetcherConfig \
	EntanglingIPrefetcherICacheSize064Sets4WaysWithPrefetcherConfig \
	EntanglingIPrefetcherICacheSize128Sets2WaysWithPrefetcherConfig \
	EntanglingIPrefetcherICacheSize128Sets4WaysWithPrefetcherConfig \
	EntanglingIPrefetcherICacheSize256Sets2WaysWithPrefetcherConfig \
	EntanglingIPrefetcherICacheSize256Sets4WaysWithPrefetcherConfig \
	EntanglingIPrefetcherICacheSize512Sets2WaysWithPrefetcherConfig \
	EntanglingIPrefetcherICacheSize512Sets4WaysWithPrefetcherConfig 

MAX_ENTANGLINGS_CONFIGS := \
	EntanglingIPrefetcherNoEntanglingConfig \
	EntanglingIPrefetcherMaxEntanglings1UncompressedConfig \
	EntanglingIPrefetcherMaxEntanglings2UncompressedConfig \
	EntanglingIPrefetcherMaxEntanglings3UncompressedConfig \
	EntanglingIPrefetcherMaxEntanglings4UncompressedConfig \
	EntanglingIPrefetcherMaxEntanglings5UncompressedConfig \
	EntanglingIPrefetcherMaxEntanglings6UncompressedConfig \
	EntanglingIPrefetcherMaxEntanglings1CompressedConfig \
	EntanglingIPrefetcherMaxEntanglings2CompressedConfig \
	EntanglingIPrefetcherMaxEntanglings3CompressedConfig \
	EntanglingIPrefetcherMaxEntanglings4CompressedConfig \
	EntanglingIPrefetcherMaxEntanglings5CompressedConfig \
	EntanglingIPrefetcherMaxEntanglings6CompressedConfig

CONFIGS := \
	EntanglingIPrefetcherNoPrefetcherBaselineConfig \
	$(TABLESIZE_CONFIGS) \
	$(CACHESIZE_CONFIGS) \
	$(MAX_ENTANGLINGS_CONFIGS)

ALL_BUILD_TARGETS             = $(patsubst %,build.%,$(CONFIGS))
TABLESIZE_BUILD_TARGETS       = $(patsubst %,build.%,$(TABLESIZE_CONFIGS))
CACHESIZE_BUILD_TARGETS       = $(patsubst %,build.%,$(CACHESIZE_CONFIGS))
MAX_ENTANGLINGS_BUILD_TARGETS = $(patsubst %,build.%,$(MAX_ENTANGLINGS_CONFIGS))

RADIOSITY_RUN_TARGETS 	  = $(patsubst %,run-radiosity.%,$(CONFIGS))
BARNES_RUN_TARGETS 		  = $(patsubst %,run-barnes.%,$(CONFIGS))
WATER_SPATIAL_RUN_TARGETS = $(patsubst %,run-water-spatial.%,$(CONFIGS))
OCEAN_CP_RUN_TARGETS      = $(patsubst %,run-ocean-cp.%,$(CONFIGS))

TABLESIZE_RUN_TARGETS       = $(patsubst %,run-radiosity.%,$(TABLESIZE_CONFIGS)) $(patsubst %,run-barnes.%,$(TABLESIZE_CONFIGS)) $(patsubst %,run-water-spatial.%,$(TABLESIZE_CONFIGS)) $(patsubst %,run-ocean-cp.%,$(TABLESIZE_CONFIGS))
CACHESIZE_RUN_TARGETS       = $(patsubst %,run-radiosity.%,$(CACHESIZE_CONFIGS)) $(patsubst %,run-barnes.%,$(CACHESIZE_CONFIGS)) $(patsubst %,run-water-spatial.%,$(CACHESIZE_CONFIGS)) $(patsubst %,run-ocean-cp.%,$(CACHESIZE_CONFIGS))
MAX_ENTANGLINGS_RUN_TARGETS = $(patsubst %,run-radiosity.%,$(MAX_ENTANGLINGS_CONFIGS)) $(patsubst %,run-barnes.%,$(MAX_ENTANGLINGS_CONFIGS)) $(patsubst %,run-water-spatial.%,$(MAX_ENTANGLINGS_CONFIGS)) $(patsubst %,run-ocean-cp.%,$(MAX_ENTANGLINGS_CONFIGS))

ALL_RUN_TARGETS 		  = $(RADIOSITY_RUN_TARGETS) $(BARNES_RUN_TARGETS) $(WATER_SPATIAL_RUN_TARGETS) $(OCEAN_CP_RUN_TARGETS)

BUILD_CONFIG 			 = $(patsubst build.%,%,$@)
RADIOSITY_RUN_CONFIG 	 = $(patsubst run-radiosity.%,%,$@)
BARNES_RUN_CONFIG 		 = $(patsubst run-barnes.%,%,$@)
WATER_SPATIAL_RUN_CONFIG = $(patsubst run-water-spatial.%,%,$@)
OCEAN_CP_RUN_CONFIG      = $(patsubst run-ocean-cp.%,%,$@)

build-all::
	$(MAKE) $(ALL_BUILD_TARGETS)

build.%::
	if [ ! -f $(BASE_DIR)/out/emulator/$(PROJECT).TestHarness/$(PROJECT).$(BUILD_CONFIG)/verilator/elf.dest/emulator ]; \
	then \
		cd $(BASE_DIR) && $(MILL) emulator[$(PROJECT).TestHarness,$(PROJECT).$(BUILD_CONFIG)].elf; \
	fi

run-all::
	$(MAKE) $(ALL_RUN_TARGETS)

run-all-tablesize::
	$(MAKE) $(TABLESIZE_RUN_TARGETS)
run-all-cachesize::
	$(MAKE) $(CACHESIZE_RUN_TARGETS)
run-all-max-entanglings::
	$(MAKE) $(MAX_ENTANGLINGS_RUN_TARGETS)

run-all.%::
	$(MAKE) run-radiosity.% run-barnes.% run-water-spatial.% run-ocean-cp.%
	
run-radiosity.%::
	mkdir -p "$(RESULTS_DIR)/$(RADIOSITY_RUN_CONFIG)"
	if [ ! -f $(RESULTS_DIR)/$(RADIOSITY_RUN_CONFIG)/radiosity.output ]; \
	then \
		$(BASE_DIR)/out/emulator/$(PROJECT).TestHarness/$(PROJECT).$(RADIOSITY_RUN_CONFIG)/verilator/elf.dest/emulator \
			pk /home/ldh35/parsec_gc/ext/splash2x/apps/radiosity/inst/amd64-linux.gcc-serial/bin/radiosity \
			-bf 1.0e-1 -batch -p 1 \
			2>&1 >"$(RESULTS_DIR)/$(RADIOSITY_RUN_CONFIG)/radiosity.output" \
			| spike-dasm >"$(RESULTS_DIR)/$(RADIOSITY_RUN_CONFIG)/radiosity.log"; \
	fi

run-barnes.%::
	mkdir -p "$(RESULTS_DIR)/$(BARNES_RUN_CONFIG)"
	if [ ! -f "$(RESULTS_DIR)/$(BARNES_RUN_CONFIG)/barnes.output" ]; \
	then \
		$(BASE_DIR)/out/emulator/$(PROJECT).TestHarness/$(PROJECT).$(BARNES_RUN_CONFIG)/verilator/elf.dest/emulator \
			pk /home/ldh35/parsec_gc/ext/splash2x/apps/barnes/inst/amd64-linux.gcc-serial/bin/barnes \
			2>&1 >"$(RESULTS_DIR)/$(BARNES_RUN_CONFIG)/barnes.output" \
			| spike-dasm >"$(RESULTS_DIR)/$(BARNES_RUN_CONFIG)/barnes.log"; \
	fi

run-water-spatial.%::
	mkdir -p "$(RESULTS_DIR)/$(WATER_SPATIAL_RUN_CONFIG)"
	if [ ! -f "$(RESULTS_DIR)/$(WATER_SPATIAL_RUN_CONFIG)/water_spatial.output" ]; \
	then \
		$(BASE_DIR)/out/emulator/$(PROJECT).TestHarness/$(PROJECT).$(WATER_SPATIAL_RUN_CONFIG)/verilator/elf.dest/emulator \
			pk /home/ldh35/parsec_gc/ext/splash2x/apps/water_spatial/inst/amd64-linux.gcc-serial/bin/water_spatial \
			"$(shell cat /home/ldh35/parsec_gc/ext/splash2x/apps/water_spatial/run/input_1)" \
			2>&1 >"$(RESULTS_DIR)/$(WATER_SPATIAL_RUN_CONFIG)/water_spatial.output" \
			| spike-dasm >"$(RESULTS_DIR)/$(WATER_SPATIAL_RUN_CONFIG)/water_spatial.log"; \
	fi

run-ocean-cp.%::
	mkdir -p "$(RESULTS_DIR)/$(OCEAN_CP_RUN_CONFIG)"
	if [ ! -f "$(RESULTS_DIR)/$(OCEAN_CP_RUN_CONFIG)/ocean_cp.output" ]; \
	then \
		$(BASE_DIR)/out/emulator/$(PROJECT).TestHarness/$(PROJECT).$(OCEAN_CP_RUN_CONFIG)/verilator/elf.dest/emulator \
			pk /home/ldh35/parsec_gc/ext/splash2x/apps/ocean_cp/inst/amd64-linux.gcc-serial/bin/ocean_cp \
			-n66 -p1 -e1e-6 -r20000.0 -t28800.0 \
			2>&1 >"$(RESULTS_DIR)/$(OCEAN_CP_RUN_CONFIG)/ocean_cp.output" \
			| spike-dasm >"$(RESULTS_DIR)/$(OCEAN_CP_RUN_CONFIG)/ocean_cp.log"; \
	fi	


	