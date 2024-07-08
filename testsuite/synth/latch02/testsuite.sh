#! /bin/sh

. ../../testenv.sh

GHDL_STD_FLAGS=--std=08
for t in nolatch1 nolatch2 nolatch3; do
    synth_only $t
done

echo "Test successful"
