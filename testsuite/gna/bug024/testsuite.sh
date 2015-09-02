#! /bin/sh

. ../../testenv.sh

analyze ppkg.vhdl
analyze_failure ppkg1.vhdl

clean

echo "Test successful"
