#! /bin/sh

. ../../testenv.sh

export GHDL_STD_FLAGS=--std=08

analyze_failure pkg.vhdl

analyze tb2.vhdl
elab_simulate tb2

analyze tb3.vhdl
elab_simulate tb3

clean

echo "Test successful"
