#! /bin/sh

. ../../testenv.sh

for t in repro repro2 repro2_1 repro3 repro4; do
    synth $t.vhdl -e $t > syn_$t.vhdl
    analyze syn_$t.vhdl
    clean
done

synth_tb repro3_1

echo "Test successful"
