#! /bin/sh

. ../../testenv.sh

export GHDL_STD_FLAGS=--std=08
analyze issue2.vhdl
elab_simulate test_issue --assert-level=error

clean

echo "Test successful"
