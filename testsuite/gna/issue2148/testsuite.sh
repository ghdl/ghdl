#! /bin/sh

. ../../testenv.sh

export GHDL_STD_FLAGS=-Werror=unused
analyze_failure unused.vhdl
analyze e.vhdl

unset GHDL_STD_FLAGS
analyze_failure e1.vhdl
analyze_failure e2.vhdl

clean

echo "Test successful"
