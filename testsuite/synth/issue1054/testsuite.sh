#! /bin/sh

. ../../testenv.sh

analyze simple01.vhdl tb_simple01.vhdl
elab_simulate tb_simple01
clean

synth simple01.vhdl -e simple01 > syn_simple01.vhdl
analyze syn_simple01.vhdl tb_simple01.vhdl
elab_simulate tb_simple01 --ieee-asserts=disable-at-0
clean

echo "Test successful"
