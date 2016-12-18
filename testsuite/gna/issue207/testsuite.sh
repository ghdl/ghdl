#! /bin/sh

. ../../testenv.sh

analyze_failure pack.vhd
analyze_failure pack1.vhd

clean

echo "Test successful"
