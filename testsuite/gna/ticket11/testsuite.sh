#! /bin/sh

. ../../testenv.sh

analyze signalevents.vhdl
elab_simulate tb

clean

echo "Test successful"
