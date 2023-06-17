#! /bin/sh

. ../../testenv.sh

verilog_synth_tb concat01
verilog_synth_tb concat02

echo "Test successful"
