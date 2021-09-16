#! /bin/sh

. ../../testenv.sh

export GHDL_STD_FLAGS=--std=08
analyze subtype_test.vhdl tb.vhdl
elab_simulate subtype_test_tb

clean

echo "Test successful"
