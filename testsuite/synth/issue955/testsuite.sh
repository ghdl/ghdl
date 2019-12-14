#! /bin/sh

. ../../testenv.sh

for f in ent; do
  synth $f.vhdl -e $f > syn_$f.vhdl
#  analyze syn_$f.vhdl
done
clean

for t in ent1; do
    analyze $t.vhdl tb_$t.vhdl
    elab_simulate tb_$t
    clean

    synth $t.vhdl -e $t > syn_$t.vhdl
    analyze syn_$t.vhdl tb_$t.vhdl
    elab_simulate tb_$t --ieee-asserts=disable-at-0
    clean
done

echo "Test successful"
