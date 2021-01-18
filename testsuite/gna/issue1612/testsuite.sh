#! /bin/sh

. ../../testenv.sh

analyze repro.vhdl
elab_simulate expose

clean

echo "Test successful"
