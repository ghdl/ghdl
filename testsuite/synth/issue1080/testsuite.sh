#! /bin/sh

. ../../testenv.sh

for t in repro repro2 repro2_1 repro3 repro4; do
    synth $t.vhdl -e $t > syn_$t.vhdl
    analyze syn_$t.vhdl
    clean
done

analyze repro3_1.vhdl tb_repro3_1.vhdl
elab_simulate tb_repro3_1
clean

synth repro3_1.vhdl -e repro3_1 > syn_repro3_1.vhdl
analyze syn_repro3_1.vhdl tb_repro3_1.vhdl
elab_simulate tb_repro3_1 --ieee-asserts=disable-at-0
clean

echo "Test successful"
