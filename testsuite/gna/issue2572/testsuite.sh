#! /bin/sh

. ../../testenv.sh

export GHDL_STD_FLAGS=--std=08
analyze fft_types.vhdl fft.vhdl tb.vhdl
elab_simulate_failure tb

clean

echo "Test successful"
