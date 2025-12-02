#! /bin/sh

. ../../testenv.sh

synth --out=dump adder.vhdl -e > syn_adder.dump

echo "Test successful"
