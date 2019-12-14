#! /bin/sh

. ../../testenv.sh

for t in pragma01; do
    analyze $t.vhdl tb_${t}_sim.vhdl
    elab_simulate tb_${t}_sim
    clean

    synth $t.vhdl -e $t > syn_$t.vhdl
    analyze syn_$t.vhdl tb_${t}_syn.vhdl
    elab_simulate tb_${t}_syn
    clean
done

echo "Test successful"
