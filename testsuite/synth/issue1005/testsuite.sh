#! /bin/sh

. ../../testenv.sh

GHDL_STD_FLAGS=--std=08
t=test
synth $t.vhdl -e $t > syn_$t.vhdl
analyze syn_$t.vhdl

clean

echo "Test successful"
