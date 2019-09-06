#! /bin/sh

. ../../testenv.sh

export GHDL_STD_FLAGS=--std=02
analyze -Wunused -Werror mwe.vhdl
elab_simulate mwe

clean

echo "Test successful"
