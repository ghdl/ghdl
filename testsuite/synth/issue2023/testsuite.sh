#! /bin/sh

. ../../testenv.sh

GHDL_FLAGS=-fsynopsys

analyze delay_bit.vhd delay_vector.vhd async_dpram.vhd abs_square.vhd test_comp.vhd
synth test_comp > syn_test_comp.vhdl

clean

echo "Test successful"
