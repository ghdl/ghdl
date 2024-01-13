#! /bin/sh

. ../../testenv.sh

export GHDL_STD_FLAGS=--std=08
analyze aggr.vhdl
elab_simulate aggregate_issue --assert-level=error

analyze aggr2.vhdl
elab_simulate aggr2

clean

echo "Test successful"
