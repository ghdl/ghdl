#! /bin/sh

. ../../testenv.sh

analyze tb.vhdl
elab_simulate tb -- +ignore
elab_simulate tb +ignore

clean

echo "Test successful"
