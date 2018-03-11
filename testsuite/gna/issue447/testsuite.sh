#! /bin/sh

. ../../testenv.sh

export GHDL_STD_FLAGS=--std=93
analyze my_entity.vhdl

clean

echo "Test successful"
