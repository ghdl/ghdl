#! /bin/sh

. ../../testenv.sh

for t in fsm_2s fsm_3s fsm_4s fsm_5s fsm_6s fsm_7s; do
    analyze $t.vhdl tb_$t.vhdl
    elab_simulate tb_$t
    clean

    synth $t.vhdl -e $t > syn_$t.vhdl
    analyze syn_$t.vhdl tb_$t.vhdl
    elab_simulate tb_$t
    clean
done

echo "Test successful"
