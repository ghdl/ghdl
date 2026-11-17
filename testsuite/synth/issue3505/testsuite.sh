#! /bin/sh

. ../../testenv.sh

GHDL_STD_FLAGS=--std=08

synth_tb mux01

echo "Test successful"
