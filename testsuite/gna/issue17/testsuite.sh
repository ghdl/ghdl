#! /bin/sh

. ../../testenv.sh

GHDL_STD_FLAGS=--std=08
analyze cond_assign_var.vhdl
elab_simulate cond_assign_var

analyze cond_assign_sig.vhdl
elab_simulate cond_assign_sig

analyze cond_assign_proc.vhdl
elab_simulate cond_assign_proc

clean

echo "Test successful"
