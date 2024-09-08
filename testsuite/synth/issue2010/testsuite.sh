#! /bin/sh

. ../../testenv.sh

synth --out=verilog -o=syn.v top.vhdl -e

synth -o=syn.vhdl top.vhdl -e
analyze syn.vhdl
clean

echo "Test successful"
