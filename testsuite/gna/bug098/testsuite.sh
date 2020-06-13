#! /bin/sh

. ../../testenv.sh

analyze_failure --std=93 loopy.vhdl

clean

echo "Test successful"
