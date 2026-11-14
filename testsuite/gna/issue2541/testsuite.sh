#! /bin/sh

. ../../testenv.sh

export GHDL_STD_FLAGS=--std=08
analyze tb.vhdl
elab_simulate tb --stop-time=50ns

clean

echo "Test successful"
