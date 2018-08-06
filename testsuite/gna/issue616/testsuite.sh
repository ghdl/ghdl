#! /bin/sh

. ../../testenv.sh

# Not very interesting, need to observe memory usage.
analyze mwe.vhdl
elab_simulate mwe

analyze repro.vhdl
analyze repro1.vhdl

clean

echo "Test successful"
