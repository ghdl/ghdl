#! /bin/sh

. ../../testenv.sh

export GHDL_STD_FLAGS=--std=08
analyze cast_enum1.vhdl
elab_simulate cast_enum1

clean

echo "Test successful"
