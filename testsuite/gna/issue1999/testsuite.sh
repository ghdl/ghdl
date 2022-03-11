#! /bin/sh

. ../../testenv.sh

analyze t1_p.vhdl
analyze_failure tc.vhdl

clean

echo "Test successful"
