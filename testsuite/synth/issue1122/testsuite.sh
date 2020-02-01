#! /bin/sh

. ../../testenv.sh

synth mult.vhd mult_pkg.vhd -e > syn_mult.vhdl
analyze mult_pkg.vhd syn_mult.vhdl
clean

for t in repro repro2; do
    synth $t.vhdl -e $t > syn_$t.vhdl
    analyze syn_$t.vhdl
    clean
done

echo "Test successful"
