#! /bin/sh

. ../../testenv.sh

analyze --std=02 bug.vhdl
analyze --std=08 bug.vhdl

GHDL_STD_FLAGS=--std=02 clean
GHDL_STD_FLAGS=--std=08 clean

echo "Test successful"
