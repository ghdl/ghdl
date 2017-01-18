#! /bin/sh

. ../../testenv.sh

export GHDL_STD_FLAGS=--std=08
analyze testcase_ce.vhdl
elab_simulate testcase_ce

clean

echo "Test successful"
