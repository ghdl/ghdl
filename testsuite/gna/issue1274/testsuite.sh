#! /bin/sh

. ../../testenv.sh

export GHDL_STD_FLAGS=--std=08
analyze_failure issue.vhdl

export GHDL_STD_FLAGS=""
analyze_failure issue93.vhdl
analyze_failure repro2.vhdl
analyze_failure repro3.vhdl
analyze_failure repro4.vhdl

echo "Test successful"
