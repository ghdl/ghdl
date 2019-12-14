#! /bin/sh

. ../../testenv.sh

analyze_failure --work=clock --warn-hide ent.vhdl

analyze lib_alias.vhdl

clean

echo "Test successful"
