#! /bin/sh

. ../../testenv.sh

export GHDL_STD_FLAGS="--std=19 --finteger64"
analyze ranges.vhdl
elab_simulate ranges

clean

echo "Test successful"
