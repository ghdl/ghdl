#! /bin/sh

. ../../testenv.sh

GHDL_STD_FLAGS=--std=08

for t in leftmost01 leftmost02 leftmost03 rightmost01 rightmost02; do
    synth_tb $t
done

synth_analyze to01_float
synth_analyze division_float

echo "Test successful"
