#! /bin/sh

. ../../testenv.sh

analyze ice.vhdl
elab_simulate e

analyze repro.vhdl
elab_simulate repro

clean

echo "Test successful"
