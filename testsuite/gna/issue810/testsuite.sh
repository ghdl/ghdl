#! /bin/sh

. ../../testenv.sh

analyze repro.vhdl
elab_simulate repro

clean

echo "Test successful"
