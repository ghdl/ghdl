#! /bin/sh

. ../../testenv.sh

GHDL_STD_FLAGS=--std=08
synth_only mwe

synth_tb alias01
synth_tb alias02

echo "Test successful"
