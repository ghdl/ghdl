#! /bin/sh

. ../../testenv.sh

analyze repro1.vhdl
elab_simulate repro1

analyze repro2.vhdl
elab_simulate repro2

clean

echo "Test successful"
