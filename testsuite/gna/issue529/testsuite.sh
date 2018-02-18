#! /bin/sh

. ../../testenv.sh

export GHDL_STD_FLAGS=--std=08
analyze impure1.vhdl
elab_simulate impure_ex

analyze impure2.vhdl
elab_simulate impure_ex2

clean

echo "Test successful"
