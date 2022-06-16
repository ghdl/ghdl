#! /bin/sh

. ../../testenv.sh

export GHDL_STD_FLAGS=--std=08
analyze repro.vhdl
elab_simulate repro

analyze dual_port_ram.vhdl
analyze fft.vhdl

clean

echo "Test successful"
