#! /bin/sh

. ../../testenv.sh

for t in slice01 slice02 slice03; do
    synth_tb $t
done

for t in slice04 slice05 slice06 slice07; do
    synth_analyze $t
done

synth_failure slice09.vhdl -e
synth_failure slice10.vhdl -e
synth_failure slice11.vhdl -e
synth_failure slice12.vhdl -e

echo "Test successful"
