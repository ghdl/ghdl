#! /bin/sh

. ../../testenv.sh

for t in asgn01 asgn02 asgn03 asgn04 asgn05 asgn06 asgn07 asgn08 arr04; do
    analyze $t.vhdl tb_$t.vhdl
    elab_simulate tb_$t
    clean

    synth $t.vhdl -e $t > syn_$t.vhdl
    analyze syn_$t.vhdl tb_$t.vhdl
    elab_simulate tb_$t
    clean
done

echo "Test successful"
