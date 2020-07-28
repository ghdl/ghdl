#! /bin/sh

. ../../testenv.sh

GHDL_STD_FLAGS="--std=08 --ieee=mentor"
analyze simple.vhdl
clean

echo "Test successful"
