#! /bin/sh

. ../../testenv.sh

GHDL_STD_FLAGS=--std=08

analyze e.vhdl

clean

echo "Test successful"
