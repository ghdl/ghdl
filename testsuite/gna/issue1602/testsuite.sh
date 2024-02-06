#! /bin/sh

. ../../testenv.sh

export GHDL_STD_FLAGS=--std=93
analyze_failure shadow.vhdl
analyze_failure repro1.vhdl

clean

echo "Test successful"
