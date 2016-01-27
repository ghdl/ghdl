#! /bin/sh

. ../../testenv.sh

analyze repro.vhdl
elab_simulate repro --dump-rti

clean

echo "Test successful"
