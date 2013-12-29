#! /bin/sh

. ../../testenv.sh

analyze t.vhdl
elab_simulate t

clean

echo "Test successful"
