#! /bin/sh

. ../../testenv.sh

verilog_synth_tb param02
verilog_synth_tb param01

echo "Test successful"
