#! /bin/sh

. ../../testenv.sh

GHDL_STD_FLAGS=--std=08

for f in restrict1 assume1 assume2 assert1; do
  synth -fpsl $f.vhdl -e $f > syn_$f.vhdl
  analyze syn_$f.vhdl
done
clean

echo "Test successful"
