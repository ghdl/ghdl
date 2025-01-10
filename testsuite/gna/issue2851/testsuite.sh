#! /bin/sh

. ../../testenv.sh

analyze circuit.vhdl
elab_failure circuit

clean

echo "Test successful"
