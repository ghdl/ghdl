#! /bin/sh

. ../../testenv.sh

analyze issue_pkg.vhdl
analyze_failure --std=93 issue.vhdl
analyze fixed.vhdl

clean

GHDL_STD_FLAGS=--std=08
analyze issue_pkg.vhdl
analyze issue.vhdl
clean

echo "Test successful"
