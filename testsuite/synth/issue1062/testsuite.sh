#! /bin/sh

. ../../testenv.sh

for t in ent; do
    synth -ggen1=5 -ggens=TRUE $t.vhdl -e $t > syn_$t.vhdl
    analyze syn_$t.vhdl tb_$t.vhdl
    elab_simulate tb_$t --ieee-asserts=disable-at-0
    clean
done

echo "Test successful"
