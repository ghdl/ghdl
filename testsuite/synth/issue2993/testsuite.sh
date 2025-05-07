#! /bin/sh

. ../../testenv.sh

GHDL_STD_FLAGS=--std=08

analyze pb1.vhdl tb_pb1.vhdl
elab_simulate tb_pb1

clean

synth --no-formal pb1.vhdl -e > syn_pb1.vhdl
analyze syn_pb1.vhdl tb_pb1.vhdl
elab_simulate tb_pb1 --ieee-asserts=disable-at-0 --assert-level=error

clean

synth_tb repro1

echo "Test successful"
