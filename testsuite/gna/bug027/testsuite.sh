#! /bin/sh

. ../../testenv.sh

analyze comp.vhdl
analyze repro.vhdl
clean

echo "Test successful"
