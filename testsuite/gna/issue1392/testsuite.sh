#! /bin/sh

. ../../testenv.sh

analyze file15.vhdl
elab_simulate file15

clean

echo "Test successful"
