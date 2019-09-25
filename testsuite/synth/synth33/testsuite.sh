#! /bin/sh

. ../../testenv.sh

for t in int_test int_test2; do
    synth $t.vhdl -e $t > syn_$t.vhdl
    analyze syn_$t.vhdl
    clean
done

echo "Test successful"
