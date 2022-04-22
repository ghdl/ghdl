#! /bin/sh

. ../../testenv.sh

export GHDL_STD_FLAGS=--std=08
analyze_failure word_entity.vhdl

analyze word_generic_pkg.vhdl
analyze word_entity.vhdl

analyze_failure word_entity1.vhdl

clean

echo "Test successful"
