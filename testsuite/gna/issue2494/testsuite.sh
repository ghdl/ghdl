#! /bin/sh

. ../../testenv.sh

export GHDL_STD_FLAGS=--std=08

if ghdl_is_preelaboration; then
    analyze compa_pkg.vhdl compa.vhdl top.vhdl tb.vhdl
    elab_simulate tb --stop-time=1us
fi

clean

echo "Test successful"
