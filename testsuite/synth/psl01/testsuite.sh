#! /bin/sh

. ../../testenv.sh

GHDL_STD_FLAGS=--std=08

for f in restrict1 restrict2 assume1 assume2 assert1 cover1 cover2 property0 sequence0; do
  synth -fpsl $f.vhdl -e $f > syn_$f.vhdl
  analyze syn_$f.vhdl
done

synth_failure cover3.vhdl -e cover3

clean

echo "Test successful"
