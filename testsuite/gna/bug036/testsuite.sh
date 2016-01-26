#! /bin/sh

. ../../testenv.sh

analyze_failure phys.vhdl
analyze phys2.vhdl

clean

echo "Test successful"
