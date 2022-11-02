#! /bin/sh

. ../../testenv.sh

GHDL_STD_FLAGS=--std=08
for t in implicit_wide implicit_wide2 implicit_wide3; do
    synth_tb $t
done

echo "Test successful"
