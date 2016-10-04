#! /bin/sh

. ../../testenv.sh

GHDL_STD_FLAGS=--std=08
analyze dictp.vhdl
analyze dictp08.vhdl

analyze -g datastructure.vhdl
analyze -g test_dict.vhdl
elab_simulate test_dict

clean

echo "Test successful"
