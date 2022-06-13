#! /bin/sh

. ../../testenv.sh

export GHDL_STD_FLAGS=--std=08
analyze_failure mypkg.vhdl
analyze_failure filpkg.vhdl

clean

echo "Test successful"
