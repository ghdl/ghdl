#! /bin/sh

. ../../testenv.sh

synth_only repro1
synth --out=verilog repro1.vhdl -e > syn_repro1.v

clean

echo "Test successful"
