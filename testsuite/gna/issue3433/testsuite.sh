#! /bin/sh

. ../../testenv.sh

export GHDL_STD_FLAGS=--std=08
analyze concat_agg.vhdl
elab_simulate concat_agg
clean

export GHDL_STD_FLAGS="--std=08 -frelaxed"
analyze concat_agg.vhdl
elab_simulate concat_agg
clean

echo "Test successful"
