#! /bin/sh

. ../../testenv.sh

analyze phys.vhdl
elab_simulate tb

analyze phys2.vhdl
elab_simulate tb2
clean

echo "Test successful"
