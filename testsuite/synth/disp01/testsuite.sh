#! /bin/sh

. ../../testenv.sh

for t in rec01 rec02 rec03 rec04 rec05 rec06; do
    analyze pkg_$t.vhdl $t.vhdl tb_$t.vhdl
    elab_simulate tb_$t
    clean

    synth pkg_$t.vhdl $t.vhdl -e $t > syn_$t.vhdl
    analyze pkg_$t.vhdl syn_$t.vhdl tb_$t.vhdl
    elab_simulate tb_$t --ieee-asserts=disable-at-0
    clean
done

echo "Test successful"
