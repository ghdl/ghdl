#! /bin/sh

. ../../testenv.sh

export GHDL_STD_FLAGS=--std=08
analyze cond_assign_proc.vhdl
elab_simulate cond_assign_proc

clean

echo "Test successful"
