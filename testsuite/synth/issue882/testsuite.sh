#! /bin/sh

. ../../testenv.sh

GHDL_STD_FLAGS=--std=08
synth cpu.vhdl -e $t > syn_cpu.vhdl
analyze syn_cpu.vhdl
clean

echo "Test successful"
