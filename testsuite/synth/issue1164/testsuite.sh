#! /bin/sh

. ../../testenv.sh

synth bug.vhdl -e > syn_bug.vhdl
analyze comp.vhdl syn_bug.vhdl

clean

echo "Test successful"
