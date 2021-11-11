#! /bin/sh

. ../../testenv.sh

GHDL_STD_FLAGS=--std=08
for t in min01 max01 uns02 urot01 srot01; do
    synth_tb $t
done

echo "Test successful"
