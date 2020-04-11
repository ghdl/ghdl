#! /bin/sh

. ../../testenv.sh

synth bug.vhdl -e > syn_bug.vhdl

echo "Test successful"
