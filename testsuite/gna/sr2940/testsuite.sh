#! /bin/sh

. ../../testenv.sh

analyze Prim.vhd
analyze_failure GCD.vhd

clean

echo "Test successful"
