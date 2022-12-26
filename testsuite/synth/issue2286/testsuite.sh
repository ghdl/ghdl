#! /bin/sh

. ../../testenv.sh

export GHDL_STD_FLAGS=--std=08

synth_tb test_addsub

echo "Test successful"
