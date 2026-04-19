#! /bin/sh

. ../../testenv.sh

synth test.vhdl -e > syn_test.vhdl

analyze anothermodule.vhdl
analyze syn_test.vhdl

clean

echo "Test successful"
