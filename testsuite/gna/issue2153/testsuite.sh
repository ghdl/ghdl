#! /bin/sh

. ../../testenv.sh

export GHDL_STD_FLAGS=--std=08
analyze issue.vhdl
elab_simulate issue

clean

echo "Test successful"
