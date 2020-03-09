#! /bin/sh

. ../../testenv.sh

GHDL_STD_FLAGS=--std=08

for t in anon01; do
    synth_tb $t
done

synth_analyze anon02

echo "Test successful"
