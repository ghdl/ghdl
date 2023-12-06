#! /bin/sh

. ../../testenv.sh

synth debounce.vhdl top.vhdl -e > syn_top.vhdl

echo "Test successful"
