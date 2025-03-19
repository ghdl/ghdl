#! /bin/sh

. ../../testenv.sh

synth repro.vhdl -e > syn_repro.vhdl
analyze syn_repro.vhdl

clean

echo "Test successful"
