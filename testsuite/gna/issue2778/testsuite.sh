#! /bin/sh

. ../../testenv.sh

export GHDL_STD_FLAGS=--std=08
analyze bug.vhdl
analyze -frelaxed bug.vhdl

clean

echo "Test successful"
