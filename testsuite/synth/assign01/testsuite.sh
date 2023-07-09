#! /bin/sh

. ../../testenv.sh

verilog_synth_tb assign01
#verilog_synth_tb assign02
verilog_synth_tb assign03
verilog_synth_tb assign05
verilog_synth_tb mem02
verilog_synth_tb mem01

echo "Test successful"
