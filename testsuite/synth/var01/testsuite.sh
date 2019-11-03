#! /bin/sh

. ../../testenv.sh

GHDL_STD_FLAGS=--std=08

for t in var01c var01b var01a var01 var02 var03 var04 var05 var06; do
    analyze $t.vhdl tb_$t.vhdl
    elab_simulate tb_$t
    clean

    synth $t.vhdl -e $t > syn_$t.vhdl
    analyze syn_$t.vhdl tb_$t.vhdl
    elab_simulate tb_$t --ieee-asserts=disable-at-0
    clean
done

echo "Test successful"
