#! /bin/sh

. ../../testenv.sh

export GHDL_STD_FLAGS=--std=08
analyze --work=libvhdl queuep.vhdl
analyze queuet.vhdl
elab_simulate queuet

clean
clean libvhdl

echo "Test successful"
