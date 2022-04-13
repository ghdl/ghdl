#! /bin/sh

. ../../testenv.sh

export GHDL_STD_FLAGS=--std=08
synth cdc_fifo_rtl.vhdl cdc_fifo_rtl_formal.psl -e > syn_cdc_fifo.vhdl

echo "Test successful"
