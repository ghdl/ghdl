#! /bin/sh

. ../../testenv.sh

analyze latches.vhdl tb_latches.vhdl
elab_simulate tb_latches

clean

synth --latches latches.vhdl -e > syn_latches.vhdl
analyze syn_latches.vhdl tb_latches.vhdl
elab_simulate tb_latches

clean

echo "Test successful"
