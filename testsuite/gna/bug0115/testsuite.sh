#! /bin/sh

. ../../testenv.sh

analyze repro1.vhdl
elab_simulate repro1 --checks

clean

echo "Test successful"
