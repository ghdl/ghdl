#! /bin/sh

. ../../testenv.sh

export GHDL_STD_FLAGS=--std=08
analyze repro.vhdl
clean

export GHDL_STD_FLAGS=--std=02
analyze repro.vhdl
clean

export GHDL_STD_FLAGS=--std=93
analyze_failure repro.vhdl
analyze -C repro.vhdl
clean

echo "Test successful"
