#! /bin/sh

. ../../testenv.sh

analyze issue1.vhdl
elab_simulate_failure issue1

analyze issue2.vhdl
elab_simulate_failure issue2

clean

export GHDL_STD_FLAGS=-Werror=runtime-error

analyze_failure issue1.vhdl
analyze_failure issue2.vhdl

clean

echo "Test successful"
