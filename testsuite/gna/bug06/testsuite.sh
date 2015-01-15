#! /bin/sh

. ../../testenv.sh

analyze repro.vhdl
elab_simulate foo

clean

echo "Test successful"
