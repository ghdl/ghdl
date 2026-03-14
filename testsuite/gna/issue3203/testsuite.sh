#! /bin/sh

. ../../testenv.sh

export GHDL_STD_FLAGS=--std=19
analyze axis.vhdl

clean

echo "Test successful"
