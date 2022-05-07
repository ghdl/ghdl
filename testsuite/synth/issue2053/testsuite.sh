#! /bin/sh

. ../../testenv.sh

GHDL_STD_FLAGS=--std=08

synth generic_fifo_fwft.vhdl generic_fifo_fwft_inst.vhdl -e > syn_generic_fifo_fwft_inst

echo "Test successful"
