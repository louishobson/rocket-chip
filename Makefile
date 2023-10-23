base_dir=$(abspath ./)

CHISEL_VERSION=3.6.0
MODEL ?= TestHarness
PROJECT ?= freechips.rocketchip.system
CONFIG ?= DefaultConfig
CONFIG_FULL ?= $(PROJECT).$(CONFIG)
MILL ?= mill

verilog:
	cd $(base_dir) && $(MILL) emulator[freechips.rocketchip.system.TestHarness,$(CONFIG_FULL)].mfccompiler.compile

bloop:
	mill --import "ivy:com.lihaoyi::mill-contrib-bloop::" mill.contrib.Bloop/install

bsp: bloop
	mill mill.bsp.BSP/install
	[ -f .bsp/mill-bsp.json ] && echo '{"name":"mill-bsp","argv":["/usr/bin/nix","develop","-c","mill","--bsp","--disable-ticker","--color","false","--jobs","1"],"millVersion":"0.10.9","bspVersion":"2.0.0","languages":["scala","java"]}' > .bsp/mill-bsp.json

vsim:
	mill emulator[freechips.rocketchip.system.TestHarness,$(CONFIG_FULL)]._

clean:
	rm -rf out/
