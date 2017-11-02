CWD = $(shell pwd)
COCOTB = $(CWD)/../../..

TOPLEVEL_LANG = vhdl

VHDL_SOURCES = $(CWD)/../hdl/top.vhd

SIM = ghdl
CMD_BIN = ghdl

TOPLEVEL=top
MODULE=$(TOPLEVEL)_cocotb

include $(COCOTB)/makefiles/Makefile.inc
include $(COCOTB)/makefiles/Makefile.sim

sim: $(MODULE).py
