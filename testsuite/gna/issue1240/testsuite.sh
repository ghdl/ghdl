#! /bin/sh

. ../../testenv.sh

export GHDL_STD_FLAGS=-Werror=runtime-error

analyze_failure issue1.vhdl
analyze_failure issue2.vhdl

clean

echo "Test successful"
