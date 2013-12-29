#! /bin/sh

. ../../testenv.sh

analyze bug2.vhdl
elab_simulate bug2

clean

echo "Test successful"
