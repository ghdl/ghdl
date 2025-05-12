#! /bin/sh

. ../../testenv.sh

synth_only top
synth --out=verilog top.vhdl -e > syn_top.v
# TODO: check the assignments in initial assign a constant value.
analyze syn_top.vhdl

echo "Test successful"
