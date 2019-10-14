#! /bin/sh

. ../../testenv.sh

for t in dff01 dff02 dff03 dff04 dff05 dff06 dff07; do
    analyze $t.vhdl tb_$t.vhdl
    elab_simulate tb_$t
    clean

    synth $t.vhdl -e $t > syn_$t.vhdl
    analyze syn_$t.vhdl tb_$t.vhdl
    elab_simulate tb_$t
    clean
done

echo "Test successful"
