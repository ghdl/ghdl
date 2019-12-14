#! /bin/sh

. ../../testenv.sh

for t in test ; do
    synth -de $t.vhdl -e > syn_$t.vhdl
    analyze syn_$t.vhdl
    clean
done

echo "Test successful"
