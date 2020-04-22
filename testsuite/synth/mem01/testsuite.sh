#! /bin/sh

. ../../testenv.sh

for t in rom1 srom01 sram01 sram02 sram03 sram05 dpram1 dpram2 dpram3; do
    analyze $t.vhdl tb_$t.vhdl
    elab_simulate tb_$t
    clean

    synth $t.vhdl -e $t > syn_$t.vhdl
    analyze syn_$t.vhdl tb_$t.vhdl
    elab_simulate tb_$t --ieee-asserts=disable-at-0
    clean
done

echo "Test successful"
