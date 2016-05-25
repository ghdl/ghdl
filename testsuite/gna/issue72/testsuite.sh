#! /bin/sh

. ../../testenv.sh

analyze issue_pkg.vhdl
analyze_failure issue.vhdl
analyze fixed.vhdl

clean

echo "Test successful"
