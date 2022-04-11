#! /bin/sh

. ../../testenv.sh

analyze --work=bugtests entity.vhdl
analyze_failure --work=bugtests testbench.vhdl 2> testbench.err
grep "prefix must designate" testbench.err | grep "testbench.vhdl"

clean bugtests

echo "Test successful"
