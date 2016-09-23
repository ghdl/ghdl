#! /bin/sh

. ../../testenv.sh

GHDL_STD_FLAGS=--std=08

analyze corelib_List.v08.vhdl
analyze corelib.v08.vhdl
analyze Integer_List_tb.vhdl
elab_simulate integer_list_tb

clean

echo "Test successful"
