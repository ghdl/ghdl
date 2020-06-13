#! /bin/sh

. ../../testenv.sh

analyze_failure --std=93 aggr1.vhdl

clean

echo "Test successful"
