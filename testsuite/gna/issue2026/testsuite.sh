#! /bin/sh

. ../../testenv.sh

export GHDL_STD_FLAGS=--std=08
analyze repro1.vhdl
clean

analyze test_tb.vhdl
elab_simulate test_tb

clean

echo "Test successful"
