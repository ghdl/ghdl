#! /bin/sh

. ../../testenv.sh

synth record_test.vhdl -e > syn_record_test.vhdl
analyze syn_record_test.vhdl

synth array_test.vhdl -e > syn_array_test.vhdl
analyze syn_array_test.vhdl

clean

echo "Test successful"
