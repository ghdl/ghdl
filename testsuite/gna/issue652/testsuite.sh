#! /bin/sh

. ../../testenv.sh

analyze_failure --work=clock --warn-hide ent.vhdl

clean

echo "Test successful"
