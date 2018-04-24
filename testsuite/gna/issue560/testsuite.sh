#! /bin/sh

. ../../testenv.sh

analyze_failure reproducer.vhdl
analyze_failure reproducer2.vhdl
analyze_failure reproducer3.vhdl
analyze reproducer_ok.vhdl

clean

echo "Test successful"
