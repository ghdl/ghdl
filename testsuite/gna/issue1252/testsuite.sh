#! /bin/sh

. ../../testenv.sh

export GHDL_STD_FLAGS=--std=02
analyze pkg.vhdl

clean

echo "Test successful"
