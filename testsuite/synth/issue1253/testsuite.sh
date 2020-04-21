#! /bin/sh

. ../../testenv.sh

GHDL_STD_FLAGS=-fsynopsys
synth repro1.vhdl -e > syn_repro1.vhdl
synth repro2.vhdl -e > syn_repro2.vhdl
clean

echo "Test successful"
