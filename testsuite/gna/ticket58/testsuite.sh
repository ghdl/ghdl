#! /bin/sh

. ../../testenv.sh

GHDL_STD_FLAGS="--std=08 --work=lib"
analyze bug.vhdl

clean

echo "Test successful"
