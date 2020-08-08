#! /bin/sh

. ../../testenv.sh

analyze mwe.vhdl
elab_simulate mwe

clean

echo "Test successful"
