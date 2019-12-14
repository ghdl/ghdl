#! /bin/sh

. ../../testenv.sh

synth int_test.vhdl -e int_test > syn_int_test.vhdl
analyze syn_int_test.vhdl
clean

echo "Test successful"
