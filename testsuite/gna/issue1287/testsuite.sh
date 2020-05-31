#! /bin/sh

. ../../testenv.sh

export GHDL_STD_FLAGS=--std=08

analyze myentity.vhdl
elab_simulate myentity

analyze myentity2.vhdl
elab_simulate myentity2

clean

echo "Test successful"
