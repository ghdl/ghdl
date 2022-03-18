#! /bin/sh

. ../../testenv.sh

export GHDL_STD_FLAGS=--std=08
synth_tb testcase

echo "Test successful"
