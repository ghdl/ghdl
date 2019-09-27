#! /bin/sh

. ../../testenv.sh

for f in ent; do
  synth --std=08 $f.vhdl -e $f > syn_$f.vhdl
  analyze syn_$f.vhdl
done
clean

echo "Test successful"
