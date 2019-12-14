#! /bin/sh

. ../../testenv.sh

for t in vhd01 vhd02; do
    analyze pkg.vhdl $t.vhdl tb_$t.vhdl
    elab_simulate tb_$t
    clean

    synth pkg.vhdl $t.vhdl -e $t > syn_$t.vhdl
    analyze pkg.vhdl syn_$t.vhdl tb_$t.vhdl
    elab_simulate tb_$t --ieee-asserts=disable-at-0
    clean
done

echo "Test successful"
