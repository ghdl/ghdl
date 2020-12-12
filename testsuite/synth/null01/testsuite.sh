#! /bin/sh

. ../../testenv.sh

GHDL_STD_FLAGS=--std=08

for t in null01 null02; do
    synth_analyze $t
done

clean

echo "Test successful"
