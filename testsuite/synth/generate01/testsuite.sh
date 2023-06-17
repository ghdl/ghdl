#! /bin/sh

. ../../testenv.sh

verilog_synth_tb genif01
verilog_synth_tb genif02
verilog_synth_tb genif03

echo "Test successful"
