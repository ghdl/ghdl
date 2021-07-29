#! /bin/sh

. ../../testenv.sh

export GHDL_STD_FLAGS=--std=08
analyze -ds entity_1.vhdl 2> entity_1.dump
cnt=$(grep -c if_generate_else entity_1.dump)
test $cnt -eq 2

clean

echo "Test successful"
