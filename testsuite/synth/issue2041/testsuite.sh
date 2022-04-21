#! /bin/sh

. ../../testenv.sh

synth --out=verilog mymodule.vhdl -e > syn_mymodule.v
grep -q default syn_mymodule.v

echo "Test successful"
