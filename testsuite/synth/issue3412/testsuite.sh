#! /bin/sh

. ../../testenv.sh

synth pkg.vhdl inner.vhdl top.vhdl -e > syn_top.vhdl

analyze pkg.vhdl syn_top.vhdl

clean

echo "Test successful"
