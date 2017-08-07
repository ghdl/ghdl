#! /bin/sh

. ../../testenv.sh

analyze e.vhdl
elab_simulate bug

clean

echo "Test successful"
