#! /bin/sh

. ../../testenv.sh

verilog_synth_tb fulladder2b

verilog_synth_tb fulladder4

echo "Test successful"
