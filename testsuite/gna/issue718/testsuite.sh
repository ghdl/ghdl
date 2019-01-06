#! /bin/sh

. ../../testenv.sh

export GHDL_STD_FLAGS=--std=08
analyze bug_repro.vhdl
elab_simulate test

clean

echo "Test successful"
