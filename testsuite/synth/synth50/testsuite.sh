#! /bin/sh

. ../../testenv.sh

for t in slv_negation; do
    synth $t.vhdl -e $t > syn_$t.vhdl
done

echo "Test successful"
