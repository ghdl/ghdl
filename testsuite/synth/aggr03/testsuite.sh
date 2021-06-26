#! /bin/sh

. ../../testenv.sh

GHDL_STD_FLAGS=--std=08
analyze conv01.vhdl tb_conv01.vhdl
elab_simulate tb_conv01
clean

synth conv01.vhdl -e conv01 > syn_conv01.vhdl
analyze syn_conv01.vhdl tb_conv01.vhdl
elab_simulate tb_conv01
clean

echo "Test successful"
