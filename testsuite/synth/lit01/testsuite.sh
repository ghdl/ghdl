#! /bin/sh

. ../../testenv.sh

analyze aggr02.vhdl tb_aggr02.vhdl
elab_simulate tb_aggr02
clean

synth aggr02.vhdl -e aggr02 > syn_aggr02.vhdl
analyze syn_aggr02.vhdl tb_aggr02.vhdl
elab_simulate tb_aggr02 --ieee-asserts=disable-at-0
clean

echo "Test successful"
