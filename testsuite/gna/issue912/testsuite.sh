#! /bin/sh

. ../../testenv.sh

GHDL_STD_FLAGS=--std=00
analyze mwe_entity.vhdl mwe_tb.vhdl
elab_simulate_failure mwe_tb

GHDL_STD_FLAGS="--std=00 --syn-binding"
analyze mwe_entity.vhdl mwe_tb.vhdl
elab_simulate mwe_tb

GHDL_STD_FLAGS=--std=02
analyze mwe_entity.vhdl mwe_tb.vhdl
elab_simulate mwe_tb

clean

echo "Test successful"
