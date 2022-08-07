#! /bin/sh

. ../../testenv.sh

export GHDL_STD_FLAGS=--std=93
analyze_failure repro.vhdl
analyze_failure repro2.vhdl
analyze_failure repro3.vhdl
analyze_failure repro4.vhdl
analyze_failure repro5.vhdl

analyze pkg_logic_misc.vhdl
analyze pkg_math_utils.vhdl
analyze_failure pkg_math_signed.vhdl

clean

echo "Test successful"
