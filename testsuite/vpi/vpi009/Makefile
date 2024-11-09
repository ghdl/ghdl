# Makefile

# defaults
SIM ?= ghdl
TOPLEVEL_LANG ?= vhdl
#SIM_ARGS ?= --wave=sim.ghw
SIM_ARGS = --vpi-trace

VHDL_SOURCES += $(PWD)/deposit_bug.vhdl \

# TOPLEVEL is the name of the toplevel module in your Verilog or VHDL file
TOPLEVEL = deposit_bug

# MODULE is the basename of the Python test file
MODULE = test_deposit_bug

# include cocotb's make rules to take care of the simulator setup
include $(shell cocotb-config --makefiles)/Makefile.sim
