#! /bin/sh

. ../../testenv.sh

GHDL_STD_FLAGS=--std=08
synth_tb ent

echo "Test successful"
