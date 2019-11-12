#! /bin/sh

. ../../testenv.sh

GHDL_STD_FLAGS=--std=08
for t in test test_orig; do
    synth -de $t.vhdl -e > syn_$t.vhdl
    analyze syn_$t.vhdl
    clean
done

echo "Test successful"
