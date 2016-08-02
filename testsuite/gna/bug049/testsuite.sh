#! /bin/sh

. ../../testenv.sh

analyze rng1.vhdl
elab_simulate sliding_index

clean

echo "Test successful"
