#! /bin/sh

. ../../testenv.sh

analyze_failure --std=02 std.vhdl

clean

echo "Test successful"
