#! /bin/sh

. ../../testenv.sh

GHDL_STD_FLAGS=--std=08
! synth mre.vhdl -e 2> mre.out
grep -q "cannot output vhdl" mre.out


echo "Test successful"
