#! /bin/sh

. ../../testenv.sh

analyze bram.vhdl tb_bram.vhdl
elab_simulate tb_bram
clean

synth bram.vhdl -e bram > syn_bram.vhdl
analyze syn_bram.vhdl tb_bram.vhdl
elab_simulate tb_bram --ieee-asserts=disable-at-0
clean

echo "Test successful"
