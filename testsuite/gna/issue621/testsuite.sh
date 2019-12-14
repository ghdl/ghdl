#! /bin/sh

. ../../testenv.sh

analyze_failure ent.vhdl
analyze_failure ent2.vhdl

clean

echo "Test successful"
