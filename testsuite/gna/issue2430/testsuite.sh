#! /bin/sh

. ../../testenv.sh

export GHDL_STD_FLAGS=--std=08
analyze my_entity.vhdl
elab_simulate my_entity

clean

echo "Test successful"
