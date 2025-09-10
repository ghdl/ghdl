#! /bin/sh

. ../../testenv.sh

analyze top.vhdl
elab_simulate -Wno-binding my_top

clean

echo "Test successful"
