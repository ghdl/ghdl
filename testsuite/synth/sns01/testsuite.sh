#! /bin/sh

. ../../testenv.sh

GHDL_STD_FLAGS=--ieee=synopsys
for t in sns01; do
    synth $t.vhdl -e $t > syn_$t.vhdl
#    analyze syn_$t.vhdl
    clean
done

echo "Test successful"
