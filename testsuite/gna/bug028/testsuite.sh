#! /bin/sh

. ../../testenv.sh

GHDL_STD_FLAGS=--std=08
GHDL_FLAGS=--ieee=mentor
analyze simple.vhdl 2>&1 | grep ignored
clean

echo "Test successful"
