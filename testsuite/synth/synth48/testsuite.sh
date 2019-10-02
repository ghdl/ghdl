#! /bin/sh

. ../../testenv.sh

GHDL_STD_FLAGS=--std=08

for f in test; do
  synth $f.vhdl -e $f > syn_$f.vhdl
  analyze syn_$f.vhdl
done
clean

echo "Test successful"
