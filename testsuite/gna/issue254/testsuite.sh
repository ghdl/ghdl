#! /bin/sh

. ../../testenv.sh

analyze e.vhdl
elab_simulate e

analyze repro1.vhdl
elab_simulate repro1

clean

echo "Test successful"
