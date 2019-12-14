#! /bin/sh

. ../../testenv.sh

for f in ent; do
  synth $f.vhdl -e $f > syn_$f.vhdl
done
clean

echo "Test successful"
