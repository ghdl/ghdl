#! /bin/sh

. ../../testenv.sh

export GHDL_STD_FLAGS=--std=08
analyze tapped_delay_line.vhdl
elab_simulate -g tapped_delay_line_tb

clean

echo "Test successful"
