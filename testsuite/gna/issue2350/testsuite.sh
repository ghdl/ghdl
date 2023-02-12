#! /bin/sh

. ../../testenv.sh

export GHDL_STD_FLAGS=--std=08
analyze test.vhdl
elab_simulate_failure test

analyze_failure -Werror=runtime-error test.vhdl
analyze_failure -Werror=runtime-error test2.vhdl

# Humm duplicate error
analyze_failure -Werror=runtime-error test3.vhdl
#analyze test3.vhdl

analyze_failure -Werror=runtime-error test4.vhdl

analyze_failure -Werror=runtime-error test5.vhdl

analyze_failure -Werror=runtime-error test6.vhdl

analyze_failure -Werror=runtime-error test7.vhdl

clean

echo "Test successful"
