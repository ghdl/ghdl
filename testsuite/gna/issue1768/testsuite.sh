#! /bin/sh

. ../../testenv.sh

analyze repro.vhdl
analyze repro2.vhdl
analyze repro3.vhdl

clean

echo "Test successful"
