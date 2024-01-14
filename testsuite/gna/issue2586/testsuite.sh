#! /bin/sh

. ../../testenv.sh

export GHDL_STD_FLAGS=--std=08

if ghdl_is_preelaboration; then
    analyze crashes.vhdl
fi

analyze repro2.vhdl
analyze repro3.vhdl
analyze repro4.vhdl

clean

echo "Test successful"
