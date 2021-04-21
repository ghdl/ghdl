#! /bin/sh

. ../../testenv.sh

GHDL_STD_FLAGS=--std=08
synth -gfifo_depth=3 fifo.vhdl axis_conv1d9x1.vhdl -e > syn_axi_conv1d9x1.vhdl

analyze syn_axi_conv1d9x1.vhdl
clean

echo "Test successful"
