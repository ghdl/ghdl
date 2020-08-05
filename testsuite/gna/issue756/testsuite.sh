#! /bin/sh

. ../../testenv.sh

export GHDL_STD_FLAGS=--std=08
analyze test_entity.vhdl
elab_simulate test_entity

analyze test_entity1.vhdl
elab_simulate test_entity

clean

echo "Test successful"
