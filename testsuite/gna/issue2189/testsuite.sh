#! /bin/sh

. ../../testenv.sh

GHDL_STD_FLAGS=--std=08
analyze bug02.vhdl
elab_simulate bug02

clean

echo "Test successful"
