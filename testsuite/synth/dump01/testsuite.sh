#! /bin/sh

. ../../testenv.sh

synth --out=dump adder.vhdl -e > syn_adder.dump

synth --out=raw adder.vhdl -e > syn_adder.raw

echo "Test successful"
