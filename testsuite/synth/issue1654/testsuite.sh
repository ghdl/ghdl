#! /bin/sh

. ../../testenv.sh

GHDL_STD_FLAGS=--std=08

# Need to disable asserts at 0.
#synth_tb checker

t=checker

analyze $t.vhdl tb_$t.vhdl
elab_simulate tb_$t
clean

synth $t.vhdl -e $t > syn_$t.vhdl
analyze syn_$t.vhdl tb_$t.vhdl
elab_simulate tb_$t --asserts=disable-at-0 --assert-level=error
clean

echo "Test successful"
