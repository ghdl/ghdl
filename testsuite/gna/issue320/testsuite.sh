#! /bin/sh

. ../../testenv.sh

analyze fuu.vhdl
elab_simulate fuu

clean

echo "Test successful"
