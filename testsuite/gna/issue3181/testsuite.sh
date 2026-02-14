#! /bin/sh

. ../../testenv.sh

export GHDL_STD_FLAGS=--finteger64
analyze t64.vhdl

clean

echo "Test successful"
