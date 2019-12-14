#! /bin/sh

. ../../testenv.sh

export GHDL_STD_FLAGS=--std=08
analyze bug_pkg.vhdl
analyze bug.vhdl

clean

export GHDL_STD_FLAGS=--std=02
analyze bug_pkg.vhdl
analyze_failure bug.vhdl

clean

echo "Test successful"
