#! /bin/sh

. ../../testenv.sh

analyze -fpsl recv.vhdl tb_recv.vhdl
elab_simulate tb_recv
clean

synth -fpsl recv.vhdl -e recv > syn_recv.vhdl
analyze syn_recv.vhdl tb_recv.vhdl
elab_simulate tb_recv
clean

echo "Test successful"
