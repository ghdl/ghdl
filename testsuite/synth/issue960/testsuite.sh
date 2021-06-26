#! /bin/sh

. ../../testenv.sh

analyze ent2.vhdl tb_ent2.vhdl
elab_simulate tb_ent2
clean

synth ent2.vhdl -e ent2 > syn_ent2.vhdl
analyze syn_ent2.vhdl tb_ent2.vhdl
elab_simulate tb_ent2 --ieee-asserts=disable-at-0
clean

synth ent.vhdl -e > syn_ent.vhdl

echo "Test successful"
