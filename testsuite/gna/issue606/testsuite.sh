#! /bin/sh

. ../../testenv.sh

analyze repro1a.vhdl repro1b.vhdl
elab_simulate repro1a

clean

echo "Test successful"
