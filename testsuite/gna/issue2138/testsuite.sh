#! /bin/sh

. ../../testenv.sh

export GHDL_STD_FLAGS=--std=08
analyze variable_assignment_with_when.vhdl
elab_simulate variable_assignment_with_when

clean

echo "Test successful"
