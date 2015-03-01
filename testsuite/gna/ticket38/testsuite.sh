#! /bin/sh

. ../../testenv.sh

analyze associate.vhdl
elab_simulate associate

clean

echo "Test successful"
