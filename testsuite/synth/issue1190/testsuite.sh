#! /bin/sh

. ../../testenv.sh

synth hdl/ram.vhdl -e > syn_ram.vhdl
analyze syn_ram.vhdl
clean

echo "Test successful"
