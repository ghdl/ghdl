#! /bin/sh

. ../../testenv.sh

analyze repro2.vhdl
elab_simulate repro2

analyze e.vhdl
elab_simulate e

clean

#export GHDL_STD_FLAGS=--std=08
analyze sim_types_pkg.vhd dma_controller_tb.vhd
elab_simulate dma_controller_tb

clean

echo "Test successful"
