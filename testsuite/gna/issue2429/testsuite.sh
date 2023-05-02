#! /bin/sh

. ../../testenv.sh

export GHDL_STD_FLAGS=--std=08
analyze repro1.vhdl
elab_simulate repro1

analyze my_entity.vhdl
elab_simulate my_entity

clean

echo "Test successful"
