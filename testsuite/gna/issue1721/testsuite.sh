#! /bin/sh

. ../../testenv.sh

export GHDL_STD_FLAGS=-fpsl

analyze top1.vhdl
elab_simulate top1

clean

echo "Test successful"
