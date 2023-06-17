#! /bin/sh

. ../../testenv.sh

verilog_synth_tb initial01
verilog_synth_tb initial02
verilog_synth_tb rom01
verilog_synth_tb rom02
verilog_synth_tb rom03
#verilog_synth_tb rom04

echo "Test successful"
