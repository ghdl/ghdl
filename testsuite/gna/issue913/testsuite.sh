#! /bin/sh

. ../../testenv.sh

export GHDL_STD_FLAGS=--std=08
analyze apackage.vhdl repro.vhdl
elab_simulate repro

clean

echo "Test successful"
