#! /bin/sh

. ../../testenv.sh

for t in simple01; do
    synth_tb $t
done

clean

synth -v simple01.vhdl -e simple01 behav > syn_simple01.vhdl

echo "Test successful"
