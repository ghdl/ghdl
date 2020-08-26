#! /bin/sh

. ../../testenv.sh

synth repro.vhdl -e > syn_repro.vhdl

echo "Test successful"
