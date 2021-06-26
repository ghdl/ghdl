#! /bin/sh

. ../../testenv.sh

analyze ent.vhdl tb_ent.vhdl
elab_simulate tb_ent
clean

synth '-gg=x"ff_ff_00_01"' ent.vhdl -e ent > syn_ent.vhdl
analyze syn_ent.vhdl tb_ent.vhdl
elab_simulate tb_ent --ieee-asserts=disable-at-0
clean

echo "Test successful"
