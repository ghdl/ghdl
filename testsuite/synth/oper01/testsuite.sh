#! /bin/sh

. ../../testenv.sh

for t in snum01 snum02 snum03 snum04 snum05 cmp01 cmp02 match01; do
    analyze $t.vhdl tb_$t.vhdl
    elab_simulate tb_$t --ieee-asserts=disable-at-0
    clean

    synth $t.vhdl -e $t > syn_$t.vhdl
    analyze syn_$t.vhdl tb_$t.vhdl
    elab_simulate tb_$t --ieee-asserts=disable-at-0
    clean
done

echo "Test successful"
