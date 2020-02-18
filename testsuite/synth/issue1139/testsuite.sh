#! /bin/sh

. ../../testenv.sh

GHDL_STD_FLAGS=--std=08
for t in ent; do
    analyze $t.vhdl tb_$t.vhdl
    elab_simulate conf
    clean

    synth $t.vhdl -e conf > syn_$t.vhdl
    analyze syn_$t.vhdl tb_$t.vhdl
    elab_simulate tb_$t --ieee-asserts=disable-at-0
    clean
done

echo "Test successful"
