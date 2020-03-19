#! /bin/sh

. ../../testenv.sh

GHDL_STD_FLAGS=--std=08
for t in issue1 issue2; do
    synth_tb $t
done

# TODO: simulation fails
synth_analyze issue3

clean

echo "Test successful"
