#! /bin/sh

. ../../testenv.sh

verilog_synth_tb module01
verilog_synth_tb module02
verilog_synth_tb module03

echo "Test successful"
