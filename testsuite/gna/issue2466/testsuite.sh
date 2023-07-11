#! /bin/sh

. ../../testenv.sh

export GHDL_STD_FLAGS=--std=93
analyze repro.vhdl
analyze repro2.vhdl

clean

export GHDL_STD_FLAGS=--std=08
analyze repro.vhdl
analyze repro2.vhdl
analyze_failure t1.vhdl
analyze t2.vhdl
analyze_failure t3.vhdl
analyze_failure t4.vhdl

clean

echo "Test successful"
