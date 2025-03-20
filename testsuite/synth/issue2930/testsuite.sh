#! /bin/sh

. ../../testenv.sh

synth_tb rom1 mem_pkg.vhdl

synth_only top

echo "Test successful"
