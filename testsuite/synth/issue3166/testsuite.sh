#! /bin/sh

. ../../testenv.sh

synth_only board

analyze gate_midff.vhdl
analyze syn_board.vhdl

synth --out=verilog board.vhdl -e > syn_board.v

clean

echo "Test successful"
