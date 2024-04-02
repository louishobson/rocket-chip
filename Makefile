base_dir=$(abspath ./)

CHISEL_VERSION=3.6.0
MODEL ?= TestHarness
PROJECT ?= freechips.rocketchip.system
CONFIG ?= DefaultConfig
CONFIG_FULL ?= $(PROJECT).$(CONFIG)
TEST_PROJECT ?= freechips.rocketchip.tiletest
TEST_CONFIG ?= EntanglingIPrefetcherUnitTestConfig
TEST_CONFIG_FULL ?= $(TEST_PROJECT).$(TEST_CONFIG)
MILL ?= mill --no-server 
RUN ?= scratch/hello_world

RISCV_CC := $(RISCV)/bin/riscv64-unknown-elf-gcc
RISCV_CARGS := -O0



bloop:
	$(MILL) --import ivy:com.lihaoyi::mill-contrib-bloop:  mill.contrib.bloop.Bloop/install

bsp: bloop
	$(MILL) mill.bsp.BSP/install
	[ -f .bsp/mill-bsp.json ] && echo '{"name":"mill-bsp","argv":["/usr/bin/nix","develop","-c","mill","--bsp","--disable-ticker","--color","false","--jobs","1"],"millVersion":"0.10.9","bspVersion":"2.0.0","languages":["scala","java"]}' > .bsp/mill-bsp.json



verilog:
	cd $(base_dir) && $(MILL) emulator[$(PROJECT).TestHarness,$(CONFIG_FULL)].mfccompiler.compile



test.elf:
	cd $(base_dir) && $(MILL) emulator[$(TEST_PROJECT).TestHarness,$(TEST_CONFIG_FULL)].elf

test.run: test.elf
	$(base_dir)/out/emulator/$(TEST_PROJECT).TestHarness/$(TEST_CONFIG_FULL)/verilator/elf.dest/emulator \
	+verbose --seed=1700375579 pk >scratch/test.log 2>&1



vsim:
	$(MILL) emulator[$(PROJECT).TestHarness,$(CONFIG_FULL)].elf

vsim.trace:
	$(MILL) emulator[$(PROJECT).TestHarness,$(CONFIG_FULL)].elf_trace



%.riscv.o: %.c
	$(RISCV_CC) $(RISCV_CARGS) -c $< -o $@

%.riscv: %.riscv.o
	$(RISCV_CC) $< -o $@

run_no_comp: $(RUN).riscv
	$(base_dir)/out/emulator/$(PROJECT).TestHarness/$(CONFIG_FULL)/verilator/elf.dest/emulator \
	+verbose \
	pk $(RUN).riscv \
	2>&1 | spike-dasm > $(RUN).log

run: vsim run_no_comp

run.trace: vsim.trace $(RUN).riscv
	$(base_dir)/out/emulator/$(PROJECT).TestHarness/$(CONFIG_FULL)/verilator/elf.dest/emulator \
	+verbose \
	-v $(RUN).vcd \
	pk $(RUN).riscv \
	2>&1 | spike-dasm > $(RUN).log

run.aux: vsim
	$(base_dir)/out/emulator/$(PROJECT).TestHarness/$(CONFIG_FULL)/verilator/elf.dest/emulator pk $(RUN) > scratch/aux.log 2>&1



clean.emulator:
	rm -rf out/emulator/$(PROJECT).TestHarness/$(CONFIG_FULL)

clean.verilator:
	rm -rf out/emulator/$(PROJECT).TestHarness/$(CONFIG_FULL)/verilator/

clean.verilog:
	rm -rf out/emulator/$(PROJECT).TestHarness/$(CONFIG_FULL)/mfccompiler/

clean.emulator.all:
	rm -rf out/emulator/

clean.test:
	rm -rf out/emulator/$(TEST_PROJECT).TestHarness

clean.test.verilator:
	rm -rf out/emulator/$(TEST_PROJECT).TestHarness/$(CONFIG_FULL)/verilator

clean.purge:
	rm -rf out/
