#! /bin/sh

. ../../testenv.sh

export GHDL_STD_FLAGS=--std=08
analyze repro.vhdl
elab_simulate ghdl_bug_repro

clean

echo "Test successful"
