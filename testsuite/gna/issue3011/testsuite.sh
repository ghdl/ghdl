#! /bin/sh

. ../../testenv.sh

export GHDL_STD_FLAGS=--std=08
analyze toto.vhdl
elab_simulate toto -gg_rst=\'1\'

clean

echo "Test successful"
