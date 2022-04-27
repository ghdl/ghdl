#! /bin/sh

. ../../testenv.sh

GHDL_STD_FLAGS=--std=08
synth_only ent3

echo "Test successful"
