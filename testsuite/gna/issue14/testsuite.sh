#! /bin/sh

. ../../testenv.sh

GHDL_STD_FLAGS=--std=08
analyze repro.vhdl
elab_simulate repro --assert-level=error

analyze repro1.vhdl
elab_simulate repro1

clean

echo "Test successful"
