#! /bin/sh

. ../../testenv.sh

GHDL_STD_FLAGS=--std=08

analyze test.vhdl tb_test.vhdl
elab_simulate tb_test
clean

synth test.vhdl -e test > syn_test.vhdl
analyze syn_test.vhdl tb_test.vhdl
elab_simulate tb_test --ieee-asserts=disable-at-0
clean

echo "Test successful"
