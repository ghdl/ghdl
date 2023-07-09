#! /bin/sh

. ../../testenv.sh

# verilog_run tb_comparator.v comparator1.v

analyze comparator1.vhdl comparator2.vhdl tb_comparator2.vhdl
elab_simulate tb_comparator2

clean


synth comparator1.v comparator2.vhdl -e > syn_comparator2.vhdl

analyze syn_comparator2.vhdl tb_comparator2.vhdl
elab_simulate tb_comparator2

clean

synth param1t.vhdl param1b.v -e > syn_param1.vhdl
analyze syn_param1.vhdl tb_param1.vhdl
elab_simulate tb_param1

clean

echo "Test successful"
