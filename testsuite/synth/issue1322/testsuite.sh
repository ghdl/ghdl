#! /bin/sh

. ../../testenv.sh

GHDL_STD_FLAGS=--std=08
analyze issue.vhdl
elab_simulate issue

synth_only issue

clean

echo "Test successful"
