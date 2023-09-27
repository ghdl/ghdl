#! /bin/sh

. ../../testenv.sh

export GHDL_STD_FLAGS=-fpsl
analyze_failure ent.vhdl

clean

echo "Test successful"
