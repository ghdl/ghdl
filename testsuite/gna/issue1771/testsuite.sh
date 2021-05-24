#! /bin/sh

. ../../testenv.sh

export GHDL_STD_FLAGS=--std=08
analyze tf.vhdl
elab_simulate tf

clean

echo "Test successful"
