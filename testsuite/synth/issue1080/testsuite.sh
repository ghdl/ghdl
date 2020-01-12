#! /bin/sh

. ../../testenv.sh

for t in repro repro2 repro2_1 repro3 repro4; do
    synth $t.vhdl -e $t > syn_$t.vhdl
    analyze syn_$t.vhdl
    clean
done

for t in repro3_1; do
    analyze $t.vhdl tb_$t.vhdl
    elab_simulate tb_$t
    clean

    synth $t.vhdl -e $t > syn_$t.vhdl
    analyze syn_$t.vhdl tb_$t.vhdl
    elab_simulate tb_$t --ieee-asserts=disable-at-0
    clean
done

echo "Test successful"
