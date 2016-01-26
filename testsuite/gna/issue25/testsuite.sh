#! /bin/sh

. ../../testenv.sh

analyze_failure 1_SecondaryUnit.vhdl
analyze 2_SecondaryUnit.vhdl

clean

echo "Test successful"
