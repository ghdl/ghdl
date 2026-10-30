#! /bin/sh

. ../../testenv.sh

export GHDL_STD_FLAGS=--std=08
analyze repro.vhdl
elab_simulate repro

analyze_failure err1.vhdl
analyze_failure err2.vhdl
analyze_failure err3.vhdl
analyze_failure err4.vhdl
analyze_failure err5.vhdl

clean

echo "Test successful"
