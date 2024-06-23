#! /bin/sh

. ../../testenv.sh

export GHDL_STD_FLAGS=--std=08
analyze testcase.vhdl
elab_simulate D

clean

echo "Test successful"
