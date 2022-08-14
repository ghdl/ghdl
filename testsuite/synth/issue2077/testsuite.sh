#! /bin/sh

. ../../testenv.sh

GHDL_STD_FLAGS=--std=08

# Not yet handled.
synth_only ent1

for t in ent2 ent3 ent5 ent6; do
    synth_tb $t
done

echo "Test successful"
