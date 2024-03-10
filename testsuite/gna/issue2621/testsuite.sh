#! /bin/sh

. ../../testenv.sh

analyze repro.vhdl
elab_simulate repro --no-run

clean

echo "Test successful"
