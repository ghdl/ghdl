#! /bin/sh

. ../../testenv.sh

export GHDL_STD_FLAGS=--std=08
analyze casetb.vhdl
elab_simulate case_questionmark_tb --assert-level=error

clean

echo "Test successful"
