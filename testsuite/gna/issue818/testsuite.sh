#! /bin/sh

. ../../testenv.sh

export GHDL_STD_FLAGS=--std=08

analyze tc1.vhdl
elab_simulate tc1

analyze tc2.vhdl
elab_simulate tc2

analyze_failure tc3.vhdl
analyze_failure tc4.vhdl
analyze_failure tc5.vhdl

analyze tc6.vhdl
elab_simulate tc6

analyze_failure tc7.vhdl
analyze_failure tc8.vhdl
analyze_failure tc9.vhdl
analyze_failure tc10.vhdl
analyze_failure tc11.vhdl
analyze_failure tc12.vhdl
analyze_failure tc13.vhdl

analyze tc14.vhdl
elab_simulate tc14

analyze_failure tc15.vhdl
analyze_failure tc16.vhdl

analyze tc17.vhdl
elab_simulate tc17

clean

echo "Test successful"
