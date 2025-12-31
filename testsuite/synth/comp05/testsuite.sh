#! /bin/sh

. ../../testenv.sh

synth_only inter01

GHDL_STD_FLAGS=--std=08
synth_only inter02

echo "Test successful"
