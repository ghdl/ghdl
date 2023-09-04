#! /bin/sh

. ../../testenv.sh

analyze repro.vhdl
analyze repro2.vhdl
analyze repro3.vhdl
elab_simulate repro3

clean

echo "Test successful"
