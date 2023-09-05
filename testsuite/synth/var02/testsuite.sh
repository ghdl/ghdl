#! /bin/sh

. ../../testenv.sh

GHDL_STD_FLAGS=--std=08

for t in var01; do
    synth_tb $t
done

echo "Test successful"
