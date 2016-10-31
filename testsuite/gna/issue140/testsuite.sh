#! /bin/sh

. ../../testenv.sh

analyze_failure --std=08 OSVVM_TB.vhd

clean

echo "Test successful"
