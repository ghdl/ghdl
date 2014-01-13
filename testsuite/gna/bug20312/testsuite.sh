#! /bin/sh

. ../../testenv.sh

analyze repro.vhdl
elab_simulate tb

analyze arr.vhdl
elab_simulate arr

clean

echo "Test successful"
