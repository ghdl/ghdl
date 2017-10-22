#! /bin/sh

. ../../testenv.sh

export GHDL_STD_FLAGS=--std=08
analyze mux4.vhdl

clean

echo "Test successful"
