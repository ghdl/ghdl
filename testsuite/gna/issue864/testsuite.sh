#! /bin/sh

. ../../testenv.sh

analyze_failure mwe.vhdl
clean

analyze_failure mwe2.vhdl
clean

echo "Test successful"
