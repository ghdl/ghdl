#! /bin/sh

. ../../testenv.sh

for t in dpram1 ram3 ram4 ram6; do
    analyze $t.vhdl tb_$t.vhdl
    elab_simulate tb_$t
    clean

    synth $t.vhdl -e $t > syn_$t.vhdl
    analyze syn_$t.vhdl tb_$t.vhdl
    elab_simulate tb_$t --ieee-asserts=disable-at-0
    clean
done

synth ram8.vhdl -e > syn_ram8.vhdl
synth ram9.vhdl -e > syn_ram9.vhdl

echo "Test successful"
