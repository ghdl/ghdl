#! /bin/sh

. ../../testenv.sh

t=record_test
synth $t.vhdl -e $t > syn_$t.vhdl
analyze syn_$t.vhdl

clean

echo "Test successful"
