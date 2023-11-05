#! /bin/sh

. ../../testenv.sh

synth --std=08 utility.vhdl inner.vhdl outer.vhdl -e > syn_outer.vhdl

echo "Test successful"
