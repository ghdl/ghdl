#! /bin/sh

. ../../testenv.sh

for t in dpram1; do
    synth_tb $t 2> $t.log
    grep "found R" $t.log
done

# Designs that doesn't create a RAM/ROM
for t in ram3 ram4 ram6; do
    synth_tb $t 2> $t.log
done

synth ram8.vhdl -e > syn_ram8.vhdl
synth ram9.vhdl -e > syn_ram9.vhdl

echo "Test successful"
