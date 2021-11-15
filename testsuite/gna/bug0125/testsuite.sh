#! /bin/sh

. ../../testenv.sh

analyze repro.vhdl tb.vhdl
elab_simulate tb

clean

echo "Test successful"
