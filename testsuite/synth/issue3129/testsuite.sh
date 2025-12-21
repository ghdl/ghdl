#! /bin/sh

. ../../testenv.sh

GHDL_STD_FLAGS=--std=08

synth_tb unstatic_loop2
clean

analyze unstatic_loop2_orig.vhdl
analyze tb_unstatic_loop2.vhdl
elab_simulate tb_unstatic_loop2
clean

echo "Test successful"
