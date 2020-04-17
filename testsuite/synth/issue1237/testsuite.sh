#! /bin/sh

. ../../testenv.sh

synth mwe.vhdl -e > syn_mwe.vhdl

echo "Test successful"
