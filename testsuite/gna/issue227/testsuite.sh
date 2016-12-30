#! /bin/sh

. ../../testenv.sh

export GHDL_STD_FLAGS=--std=08
analyze repro.vhdl
elab_simulate test

analyze --ieee=synopsys test.vhdl
elab_simulate --ieee=synopsys test

clean

echo "Test successful"
