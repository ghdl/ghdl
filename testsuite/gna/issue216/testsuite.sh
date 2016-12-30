#! /bin/sh

. ../../testenv.sh

export GHDL_STD_FLAGS=--std=08
analyze test.vhdl
elab_simulate test

analyze repro1.vhdl
elab_simulate repro1

analyze repro2.vhdl
analyze repro3.vhdl
clean

echo "Test successful"
