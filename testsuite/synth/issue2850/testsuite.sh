#! /bin/sh

. ../../testenv.sh

GHDL_STD_FLAGS=--std=08
synth_tb issue

clean

echo "Test successful"
