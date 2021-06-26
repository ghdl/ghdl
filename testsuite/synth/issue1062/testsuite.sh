#! /bin/sh

. ../../testenv.sh

synth -ggen1=5 -ggens=TRUE ent.vhdl -e ent > syn_ent.vhdl
analyze syn_ent.vhdl tb_ent.vhdl
elab_simulate tb_ent --ieee-asserts=disable-at-0
clean

echo "Test successful"
