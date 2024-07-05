#! /bin/sh

. ../../testenv.sh

GHDL_STD_FLAGS=--std=08

for f in slow slow-orig fast; do
    synth_only $f
done

echo "Test successful"
