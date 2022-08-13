#! /bin/sh

. ../../testenv.sh

GHDL_STD_FLAGS=--std=08

for t in ent2 ent3 ent5; do
    synth_tb $t
done

echo "Test successful"
