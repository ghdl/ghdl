#! /bin/sh

. ../../testenv.sh

synth --expect-failure ent1.vhdl -e

for t in ent2; do
    synth $t.vhdl -e $t > syn_$t.vhdl
    analyze syn_$t.vhdl
    clean
done

echo "Test successful"
