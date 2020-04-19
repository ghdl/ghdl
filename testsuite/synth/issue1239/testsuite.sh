#! /bin/sh

. ../../testenv.sh

GHDL_STD_FLAGS=-fsynopsys
synth repro.vhdl -e > syn_repro.vhdl

echo "Test successful"
