#! /bin/sh

. ../../testenv.sh

export GHDL_STD_FLAGS=--std=08
$GHDL -s $GHDL_STD_FLAGS alias.vhdl

clean

echo "Test successful"
