#! /bin/sh

. ../../testenv.sh

analyze repro.vhdl
elab_simulate foo_tb

analyze tb.vhdl
elab_simulate foo_tb

clean

echo "Test successful"
