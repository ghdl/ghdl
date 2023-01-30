#! /bin/sh

. ../../testenv.sh

GHDL_STD_FLAGS=--work=bugtests
analyze empty.vhdl
elab_simulate abcdefg_tb

clean

echo "Test successful"
