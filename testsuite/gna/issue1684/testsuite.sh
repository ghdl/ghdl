#! /bin/sh

. ../../testenv.sh

analyze pkg.vhdl
analyze ent1.vhdl

clean

echo "Test successful"
