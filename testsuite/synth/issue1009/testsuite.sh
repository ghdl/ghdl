#! /bin/sh

. ../../testenv.sh

synth record_test.vhdl -e > syn_record_test.vhdl
analyze syn_record_test.vhdl

clean

echo "Test successful"
