#! /bin/sh

. ../../testenv.sh

GHDL_STD_FLAGS=--std=08

synth_analyze issue1899

clean

echo "Test successful"
