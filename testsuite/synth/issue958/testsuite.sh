#! /bin/sh

. ../../testenv.sh

for f in ent ent1; do
  synth $f.vhdl -e $f > syn_$f.vhdl
  analyze c.vhdl syn_$f.vhdl
  clean
done

echo "Test successful"
