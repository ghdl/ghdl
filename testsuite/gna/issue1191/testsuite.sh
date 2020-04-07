#! /bin/sh

. ../../testenv.sh

export GHDL_STD_FLAGS="--std=08 -frelaxed"
analyze mux_fifo_pkg.vhd mux_fifo.vhd mux_fifo_tb.vhd
elab_simulate mux_fifo_tb

clean

echo "Test successful"
