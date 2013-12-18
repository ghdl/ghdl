#! /bin/sh

. ../../testenv.sh

analyze pkg.vhd
analyze top.vhdl
elab_simulate top

clean

echo "Test successful"
