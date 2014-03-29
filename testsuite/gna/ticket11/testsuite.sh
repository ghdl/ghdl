#! /bin/sh

. ../../testenv.sh

analyze signalevents.vhdl
elab_simulate tb

analyze tb2.vhdl
elab_simulate tb2

analyze tb3.vhdl
elab_simulate tb3

clean

echo "Test successful"
