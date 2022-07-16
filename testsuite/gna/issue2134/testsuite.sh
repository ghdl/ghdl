#! /bin/sh

. ../../testenv.sh

export GHDL_STD_FLAGS=--std=08
analyze repro1.vhdl
analyze attribute_on_shared_variable.vhdl

clean

echo "Test successful"
