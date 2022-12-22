#! /bin/sh

. ../../testenv.sh

export GHDL_STD_FLAGS=--std=08

synth_only ent

synth_tb aggr1

echo "Test successful"
