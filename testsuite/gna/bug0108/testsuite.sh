#! /bin/sh

. ../../testenv.sh

analyze pkg.vhdl
analyze ent.vhdl

clean

echo "Test successful"
