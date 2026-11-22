#! /bin/sh

. ../../testenv.sh

synth --out=verilog --latches clk_gate.vhdl -e > syn_clk_gate.vhdl

grep -q -e "if (n.) n. <= n.;" syn_clk_gate.vhdl

echo "Test successful"
