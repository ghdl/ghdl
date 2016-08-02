#! /bin/sh

. ../../testenv.sh

analyze adder.vhdl
elab_simulate csac

clean

echo "Test successful"
