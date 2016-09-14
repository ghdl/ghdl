#! /bin/sh

. ../../testenv.sh

GHDL_STD_FLAGS=--std=87
analyze concat2.vhdl
elab_simulate_failure concat
clean

GHDL_STD_FLAGS=--std=93
analyze concat2.vhdl
elab_simulate concat
clean

echo "Test successful"
