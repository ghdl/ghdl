#! /bin/sh

. ../../testenv.sh

export GHDL_STD_FLAGS=--std=08
analyze repro1.vhdl
analyze repro2.vhdl

analyze repro3.vhdl
elab_simulate repro3

clean

echo "Test successful"
