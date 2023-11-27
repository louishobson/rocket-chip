base_dir=$(abspath ./)

CHISEL_VERSION=3.6.0
MODEL ?= TestHarness
PROJECT ?= freechips.rocketchip.system
CONFIG ?= DefaultConfig
CONFIG_FULL ?= $(PROJECT).$(CONFIG)
MILL ?= mill
RUN ?= scratch/hello_world

RISCV_CC := $(RISCV)/bin/riscv64-unknown-elf-gcc
RISCV_CARGS := -O0



bloop:
	mill --import "ivy:com.lihaoyi::mill-contrib-bloop::" mill.contrib.Bloop/install

bsp: bloop
	mill mill.bsp.BSP/install
	[ -f .bsp/mill-bsp.json ] && echo '{"name":"mill-bsp","argv":["/usr/bin/nix","develop","-c","mill","--bsp","--disable-ticker","--color","false","--jobs","1"],"millVersion":"0.10.9","bspVersion":"2.0.0","languages":["scala","java"]}' > .bsp/mill-bsp.json



verilog:
	cd $(base_dir) && $(MILL) emulator[freechips.rocketchip.system.TestHarness,$(CONFIG_FULL)].mfccompiler.compile



test.elf.ent:
	cd $(base_dir) && $(MILL) emulator[freechips.rocketchip.unittest.TestHarness,freechips.rocketchip.unittest.EntanglerUnitTestConfig].elf

test.run.ent: test.elf.ent
	$(base_dir)/out/emulator/freechips.rocketchip.unittest.TestHarness/freechips.rocketchip.unittest.EntanglerUnitTestConfig/verilator/elf.dest/emulator \
	+verbose pk >scratch/test.ent.log 2>&1



vsim:
	mill emulator[freechips.rocketchip.system.TestHarness,$(CONFIG_FULL)].elf

vsim.trace:
	mill emulator[freechips.rocketchip.system.TestHarness,$(CONFIG_FULL)].elf_trace



%.riscv.o: %.c
	$(RISCV_CC) $(RISCV_CARGS) -c $< -o $@

%.riscv: %.riscv.o
	$(RISCV_CC) $< -o $@

run: vsim $(RUN).riscv
	$(base_dir)/out/emulator/freechips.rocketchip.system.TestHarness/$(CONFIG_FULL)/verilator/elf.dest/emulator \
	+verbose \
	pk $(RUN).riscv \
	2>&1 | spike-dasm > $(RUN).log

run.trace: vsim.trace $(RUN).riscv
	$(base_dir)/out/emulator/freechips.rocketchip.system.TestHarness/$(CONFIG_FULL)/verilator/elf.dest/emulator \
	+verbose \
	-v $(RUN).vcd \
	pk $(RUN).riscv \
	2>&1 | spike-dasm > $(RUN).log



clean.emulator:
	rm -r out/emulator/freechips.rocketchip.system.TestHarness/$(CONFIG_FULL)

clean.verilator:
	rm -r out/emulator/freechips.rocketchip.system.TestHarness/$(CONFIG_FULL)/verilator

clean.verilog:
	rm -r out/emulator/freechips.rocketchip.system.TestHarness/$(CONFIG_FULL)/mfccompiler

clean.emulator.all:
	rm -r out/emulator/

clean.test:
	rm -r out/emulator/freechips.rocketchip.unittest.TestHarness

clean.purge:
	rm -r out/
