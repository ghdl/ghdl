#! /bin/sh

. ../../testenv.sh

#export GHDL_STD_FLAGS=--std=08
analyze crash2.vhdl
analyze generics.vhdl

clean

echo "Test successful"
