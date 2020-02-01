#! /bin/sh

. ../../testenv.sh

for t in dff05 dff06 dff08 dff08a dff08b dff08c dff08d dff09; do
    analyze $t.vhdl tb_$t.vhdl
    elab_simulate tb_$t
    clean

    synth $t.vhdl -e $t > syn_$t.vhdl
    analyze syn_$t.vhdl tb_$t.vhdl
    elab_simulate tb_$t
    clean
done

echo "Test successful"
