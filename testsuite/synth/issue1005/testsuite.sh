#! /bin/sh

. ../../testenv.sh

GHDL_STD_FLAGS=--std=08
for t in test test2; do
  synth $t.vhdl -e $t > syn_$t.vhdl
  analyze syn_$t.vhdl
done

clean

echo "Test successful"
