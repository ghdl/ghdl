#! /bin/sh

. ../../testenv.sh

export GHDL_STD_FLAGS=--std=08
analyze repro.vhdl
elab_simulate repro

analyze repro2.vhdl
elab_simulate repro2

analyze repro4.vhdl
elab_simulate repro4

clean

echo "Test successful"
