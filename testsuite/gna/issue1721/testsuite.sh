#! /bin/sh

. ../../testenv.sh

export GHDL_STD_FLAGS=-fpsl

analyze top1.vhdl
elab_simulate top1

analyze_failure top2.vhdl

clean

echo "Test successful"
