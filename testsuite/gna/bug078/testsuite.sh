#! /bin/sh

. ../../testenv.sh

analyze half_adder.vhdl
elab_simulate different

clean

echo "Test successful"
