#! /bin/sh

. ../../testenv.sh

for f in test test2; do
  synth $f.vhdl -e $f > syn_$f.vhdl
  analyze syn_$f.vhdl
  clean
done

echo "Test successful"
