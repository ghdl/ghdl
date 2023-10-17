#! /bin/sh

. ../../testenv.sh

analyze repro1_1.vhdl repro1_2.vhdl repro1.vhdl
elab_simulate repro1

clean

echo "Test successful"
