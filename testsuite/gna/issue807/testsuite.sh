#! /bin/sh

. ../../testenv.sh

analyze repropoc.vhdl
elab_simulate repropoc

analyze reproct.vhdl
elab_simulate reproct

clean

export GHDL_STD_FLAGS=--std=08
analyze test.vhdl repro.vhdl
elab_simulate test

clean

echo "Test successful"
