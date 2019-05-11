#! /bin/sh

. ../../testenv.sh

export GHDL_STD_FLAGS=--std=08
analyze_failure -frelaxed repro1.vhdl

clean

echo "Test successful"
