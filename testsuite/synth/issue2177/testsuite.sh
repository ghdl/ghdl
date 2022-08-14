#! /bin/sh

. ../../testenv.sh


synth --out=verilog vlm5030_pack.vhd clock_functions_pack.vhd vlm5030_subcircuits.vhd vlm5030_gl.vhd -e > syn_vlm5030.v

echo "Test successful"
