#! /bin/sh

. ../../testenv.sh

export GHDL_STD_FLAGS=--std=08
analyze dut.vhdl
analyze test.vhdl
elab_simulate test

analyze repro.vhdl
elab_simulate repro

clean

echo "Test successful"
