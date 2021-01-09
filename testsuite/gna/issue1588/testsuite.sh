#! /bin/sh

. ../../testenv.sh

analyze tent.vhdl
analyze_failure libs12.vhdl

clean

echo "Test successful"
