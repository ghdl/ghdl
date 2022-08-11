#! /bin/sh

. ../../testenv.sh

export GHDL_STD_FLAGS=--std=08
run "$GHDL" -s $GHDL_STD_FLAGS test1.vhdl
run "$GHDL" -s $GHDL_STD_FLAGS test2.vhdl

clean

echo "Test successful"
