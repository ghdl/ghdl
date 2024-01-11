#! /bin/sh

. ../../testenv.sh

export GHDL_STD_FLAGS=--std=08
analyze aggr.vhdl
elab_simulate aggregate_issue --assert-level=error

clean

echo "Test successful"
