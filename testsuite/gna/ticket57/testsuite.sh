#! /bin/sh

. ../../testenv.sh

analyze test.vhdl
analyze_failure --std=93 test.vhdl
analyze --std=93 -frelaxed-rules test.vhdl

clean

echo "Test successful"
