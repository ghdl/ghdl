#! /bin/sh

. ../../testenv.sh

analyze e.vhdl
elab_simulate e

clean

echo "Test successful"
