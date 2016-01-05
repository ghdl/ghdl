#! /bin/sh

. ../../testenv.sh

analyze_failure --std=93 test_id.vhdl
analyze test_id.vhdl

clean

echo "Test successful"
