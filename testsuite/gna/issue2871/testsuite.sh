#! /bin/sh

. ../../testenv.sh

analyze example.vhdl
elab_simulate example -gparameter=11110001

clean

echo "Test successful"
