#! /bin/sh

. ../../testenv.sh

analyze dummy.vhdl
elab_simulate dummy

clean

echo "Test successful"
