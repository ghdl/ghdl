#! /bin/sh

. ../../testenv.sh

export GHDL_STD_FLAGS=--std=08
synth -Werror cdc_fifo_rtl.vhdl -e > syn_cdc_fifo_rtl.vhdl

echo "Test successful"
