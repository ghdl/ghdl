#! /bin/sh

. ../../testenv.sh

synth top_pkg.vhdl top.vhdl -e top > syn_top.vhdl
analyze top_pkg.vhdl syn_top.vhdl
clean

echo "Test successful"
