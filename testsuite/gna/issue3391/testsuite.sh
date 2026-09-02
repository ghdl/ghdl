#! /bin/sh

. ../../testenv.sh

export GHDL_STD_FLAGS=--std=93
analyze delay.vhdl top.vhdl
elab_failure top

clean

export GHDL_STD_FLAGS=--std=08
analyze delay.vhdl top.vhdl
elab_simulate top --stop-time=1us

clean

echo "Test successful"
