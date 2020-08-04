#! /bin/sh

. ../../testenv.sh

GHDL_STD_FLAGS=--std=08

analyze repro1.vhdl
if ghdl_has_feature repro1 dump-rti; then
    elab_simulate repro1 --dump-rti
fi

clean

echo "Test successful"
