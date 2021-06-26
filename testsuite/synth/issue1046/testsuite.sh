#! /bin/sh

. ../../testenv.sh

analyze concat01.vhdl tb_concat01.vhdl
elab_simulate tb_concat01
clean

synth concat01.vhdl -e concat01 > syn_concat01.vhdl
analyze syn_concat01.vhdl tb_concat01.vhdl
elab_simulate tb_concat01 --ieee-asserts=disable-at-0
clean

echo "Test successful"
