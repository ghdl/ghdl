#! /bin/sh

. ../../testenv.sh

analyze repro1.vhdl
elab_simulate repro1

clean

echo "Test successful"
