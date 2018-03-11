#! /bin/sh

. ../../testenv.sh

analyze sig.vhdl
elab_simulate sig

clean

echo "Test successful"
