#!/bin/sh

. ../../testenv.sh

GHDL_STD_FLAGS="--std=08"
synth_tb test

clean

echo "Test successful"
