#! /bin/sh

. ../../testenv.sh

GHDL_STD_FLAGS=--std=08

analyze testcase.vhdl tb_testcase.vhdl
elab_simulate tb_testcase
clean

synth testcase.vhdl -e testcase > syn_testcase.vhdl
analyze syn_testcase.vhdl tb_testcase.vhdl
elab_simulate tb_testcase
clean

echo "Test successful"
