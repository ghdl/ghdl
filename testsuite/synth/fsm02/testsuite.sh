#! /bin/sh

. ../../testenv.sh

for t in recv; do
    analyze -fpsl $t.vhdl tb_$t.vhdl
    elab_simulate tb_$t
    clean

    synth -fpsl $t.vhdl -e $t > syn_$t.vhdl
    analyze syn_$t.vhdl tb_$t.vhdl
    elab_simulate tb_$t
    clean
done

echo "Test successful"
