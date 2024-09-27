#! /bin/sh

. ../../testenv.sh

analyze top.vhdl
elab_simulate top

clean

echo "Test successful"
