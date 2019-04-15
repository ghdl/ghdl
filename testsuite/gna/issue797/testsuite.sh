#! /bin/sh

. ../../testenv.sh

analyze repro.vhdl
elab_simulate repro

analyze pkg_c.vhdl

clean

echo "Test successful"
