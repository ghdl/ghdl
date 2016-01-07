#! /bin/sh

. ../../testenv.sh

GHDL_STD_FLAGS=--std=08
analyze to_slv_issue.vhdl
elab_simulate to_slv_issue

clean

echo "Test successful"
