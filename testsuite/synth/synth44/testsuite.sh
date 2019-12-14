#! /bin/sh

. ../../testenv.sh

for f in record_test; do
  synth $f.vhdl -e $f > syn_$f.vhdl
#  analyze syn_$f.vhdl
done
clean

echo "Test successful"
