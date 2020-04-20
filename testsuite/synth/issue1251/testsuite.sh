#! /bin/sh

. ../../testenv.sh

synth theunit.vhdl -e > syn_theunit.vhdl

echo "Test successful"
