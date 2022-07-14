#! /bin/sh

. ../../testenv.sh

GHDL_SYNTH_FLAGS=--latches

for t in latch01; do
    synth_tb $t
done

echo "Test successful"
