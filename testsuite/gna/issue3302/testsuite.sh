#! /bin/sh

. ../../testenv.sh

if ghdl_is_preelaboration; then

export GHDL_STD_FLAGS=--std=08
analyze a.vhdl
elab_simulate c

analyze a2.vhdl
elab_simulate_failure c2

clean

fi

echo "Test successful"
