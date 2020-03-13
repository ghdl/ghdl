#! /bin/sh

. ../../testenv.sh

for t in iassoc01 iassoc02 iassoc03 iassoc04 iassoc11 iassoc12; do
    analyze pkg.vhdl $t.vhdl tb_$t.vhdl
    elab_simulate tb_$t
    clean

    synth pkg.vhdl $t.vhdl -e $t > syn_$t.vhdl
    analyze syn_$t.vhdl tb_$t.vhdl
    elab_simulate tb_$t --ieee-asserts=disable-at-0
    clean
done

echo "Test successful"
