#! /bin/sh

. ../../testenv.sh

GHDL_STD_FLAGS=--std=08
analyze seg_fault.vhdl
elab e

# Do not simulate: infinite recursion.

clean

echo "Test successful"
