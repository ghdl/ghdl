#! /bin/sh

. ../../testenv.sh

export GHDL_STD_FLAGS=--std=08
analyze repro1.vhdl
elab_simulate repro1

clean

export GHDL_STD_FLAGS=--std=93

analyze repro2.vhdl
elab_simulate repro2

clean

echo "Test successful"
