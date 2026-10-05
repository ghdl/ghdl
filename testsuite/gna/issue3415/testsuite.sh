#! /bin/sh

. ../../testenv.sh

export GHDL_STD_FLAGS=--std=19
analyze repro1.vhdl
analyze repro2.vhdl
analyze axiliteinterface.vhdl top.vhdl outputtest.vhdl

clean

echo "Test successful"
