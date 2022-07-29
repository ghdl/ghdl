#! /bin/sh

. ../../testenv.sh

export GHDL_STD_FLAGS="--std=08 -Werror=no-wait"

analyze e.vhdl
analyze e1.vhdl
analyze e2.vhdl
analyze e3.vhdl
analyze_failure e4.vhdl

clean

echo "Test successful"
