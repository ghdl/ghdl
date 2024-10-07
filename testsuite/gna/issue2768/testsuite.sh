#! /bin/sh

. ../../testenv.sh

analyze_failure --std=08 --force-analysis bad_inter.vhdl
analyze_failure --std=08 --force-analysis bad_inter2.vhdl
analyze_failure --std=08 --force-analysis bad_inter3.vhdl
analyze_failure --std=08 --force-analysis many_parent.vhdl
analyze_failure --std=08 --force-analysis missing_id.vhdl

clean

echo "Test successful"
