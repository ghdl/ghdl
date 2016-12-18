#! /bin/sh

. ../../testenv.sh

GHDL_STD_FLAGS=--std=08

analyze testpkg.vhdl
analyze test1.vhdl

clean

echo "Test successful"
