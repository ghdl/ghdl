#! /bin/sh

. ../../testenv.sh

export GHDL_STD_FLAGS=--std=08
# Do not try to generate code, it is currently not supported.
$GHDL -s $GHDL_STD_FLAGS source.vhdl

clean

echo "Test successful"
