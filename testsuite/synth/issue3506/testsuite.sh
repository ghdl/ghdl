#! /bin/sh

. ../../testenv.sh

GHDL_STD_FLAGS=--std=08

for t in dynslice01 dynslice02 dynslice03; do
    synth_tb $t
done

echo "Test successful"
