#! /bin/sh

. ../../testenv.sh

analyze example.vhdl
elab_simulate example -gparameter=1001

clean

echo "Test successful"
