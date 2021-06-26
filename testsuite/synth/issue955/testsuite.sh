#! /bin/sh

. ../../testenv.sh

for f in ent; do
  synth $f.vhdl -e $f > syn_$f.vhdl
#  analyze syn_$f.vhdl
done
clean

synth_tb ent1

echo "Test successful"
