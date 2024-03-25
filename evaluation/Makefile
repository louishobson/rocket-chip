EVAL_DIR := $(abspath ./)
BASE_DIR := $(abspath ../)

TARGETS := \
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

MILL := mill --no-server 
PROJECT := freechips.rocketchip.system
FAIL_SLEEP := 20 

build-all: $(patsubst %,build-%,$(TARGETS))

build-%::
	cd $(BASE_DIR) && until $(MILL) emulator[$(PROJECT).TestHarness,$(PROJECT).$(patsubst build-%,%,$@)].elf; do sleep $(FAIL_SLEEP); done
	