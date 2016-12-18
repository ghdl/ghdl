#! /bin/sh

. ../../testenv.sh

export GHDL_STD_FLAGS=--std=08
analyze --work=osvvm ScoreboardGenericPkg.vhd

analyze repro.vhdl
elab_simulate e

clean
clean osvvm

echo "Test successful"
