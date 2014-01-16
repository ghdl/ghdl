#! /bin/sh

. ../../testenv.sh

analyze repro.vhdl

elab_simulate top

clean

echo "Test successful"
