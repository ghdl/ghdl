#! /bin/sh

. ../../testenv.sh

export GHDL_STD_FLAGS=--std=08
analyze_failure repro.vhdl
analyze_failure crc_pkg.vhdl wbcrc_syn.vhdl

export GHDL_STD_FLAGS="--std=08 -frelaxed"
analyze repro.vhdl
elab_simulate repro
analyze crc_pkg.vhdl wbcrc_syn.vhdl

clean

echo "Test successful"
