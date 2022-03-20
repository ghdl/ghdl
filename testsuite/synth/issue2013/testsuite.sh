#! /bin/sh

. ../../testenv.sh

GHDL_STD_FLAGS=--std=08
for t in testcase tc3 tc2 tc4; do
    synth_tb $t
done

echo "Test successful"
