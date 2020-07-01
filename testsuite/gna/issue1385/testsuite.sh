#! /bin/sh

. ../../testenv.sh

analyze_failure --std=93 ex1.vhdl
analyze ex1.vhdl

clean

echo "Test successful"
