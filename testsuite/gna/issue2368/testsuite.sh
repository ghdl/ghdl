#! /bin/sh

. ../../testenv.sh

export GHDL_STD_FLAGS=--std=08
analyze matchent1.vhdl
analyze matchent2.vhdl

clean

echo "Test successful"
