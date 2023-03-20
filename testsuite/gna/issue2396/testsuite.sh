#! /bin/sh

. ../../testenv.sh

export GHDL_STD_FLAGS=--std=08
analyze frequency.vhdl
analyze tb_freq.vhdl
elab_simulate tb_freq --stop-time=1us

clean

echo "Test successful"
