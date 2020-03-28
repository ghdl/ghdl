#! /bin/sh

. ../../testenv.sh

GHDL_STD_FLAGS=-fsynopsys

for t in sns01; do
    synth $t.vhdl -e $t > syn_$t.vhdl
    # No analysis because of conflict between numeric_std.unsigned and
    # std_logic_arith.unsigned
#    analyze syn_$t.vhdl
    clean
done

synth_analyze sns02
clean

echo "Test successful"
