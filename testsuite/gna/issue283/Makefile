PWD=$(shell pwd)

# Adjust to your needs:
# ======================================================================================================================

# Path to the root folder of the simulation framework
SIM_FRAMEWORK_DIR = /home/kaolpr/simulation_framework

# Top VHDL entity
TOPLEVEL = foo

# Python test file name (without extension)
MODULE = example

# Waveform output file
WAVEFORM_OUTPUT = waveform.ghw

# (Typically) Not to be modified:
# ======================================================================================================================

UNISIM_DIR = $(SIM_FRAMEWORK_DIR)/lib/xilinx-vivado/unisim/v08/
VHDL_VERSION = 08

VHDL_SOURCES = $(shell pwd)/bar.vhd $(shell pwd)/uut.vhd

COCOTB = $(SIM_FRAMEWORK_DIR)/lib/cocotb
GHDL_BIN_DIR = $(SIM_FRAMEWORK_DIR)/lib/ghdl/build/bin/

SIM=ghdl
ANALYSE_ARGS =
ELABORATE_ARGS =
SIM_ARGS = --wave=$(WAVEFORM_OUTPUT) --ieee-asserts=disable

PYTHON_SIM_MODULES_PATH = $(SIM_FRAMEWORK_DIR)/lib/python_modules

include $(COCOTB)/makefiles/Makefile.inc
include $(COCOTB)/makefiles/Makefile.sim
