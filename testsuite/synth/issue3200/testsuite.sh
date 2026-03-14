#! /bin/sh

. ../../testenv.sh

synth --out=verilog test.vhdl -e > syn_test.v

fgrep -q '(* testarch="hello 2" *) module test' syn_test.v

echo "Test successful"
