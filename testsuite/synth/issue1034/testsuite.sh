#! /bin/sh

. ../../testenv.sh

for t in ent; do
    analyze p.vhdl $t.vhdl tb_$t.vhdl
    elab_simulate tb_$t
    clean

    synth p.vhdl $t.vhdl -e $t > syn_$t.vhdl
    analyze p.vhdl syn_$t.vhdl tb_$t.vhdl
    elab_simulate tb_$t --ieee-asserts=disable-at-0
    clean
done

echo "Test successful"
