#! /bin/sh

. ../../testenv.sh

GHDL_STD_FLAGS=--std=08
for t in test; do
  synth $t.vhdl -e $t > syn_$t.vhdl
  analyze syn_$t.vhdl
done

for t in test2; do
    analyze $t.vhdl tb_$t.vhdl
    elab_simulate tb_$t
    clean

    synth $t.vhdl -e $t > syn_$t.vhdl
    analyze syn_$t.vhdl tb_$t.vhdl
    elab_simulate tb_$t --ieee-asserts=disable-at-0
    clean
done

clean

echo "Test successful"
