#! /bin/sh

. ../../testenv.sh

export GHDL_STD_FLAGS=--std=08
analyze repro.vhdl
elab_simulate repro

analyze tst.vhdl
elab_simulate tst

clean

echo "Test successful"
