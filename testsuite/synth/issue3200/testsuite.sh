#! /bin/sh

. ../../testenv.sh

synth --out=verilog test.vhdl -e > syn_test.v

# Attribute on architecture
fgrep -q '(* testarch="hello 2" *) module test' syn_test.v

# Attribute on component instantiation
fgrep -q '(* testlabel="hello world" *) anothermodule myinst' syn_test.v

echo "Test successful"
