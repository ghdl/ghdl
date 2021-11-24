#! /bin/sh

. ../../testenv.sh

export GHDL_STD_FLAGS=--std=08

analyze delayline1d.vhdl tb_delayline1d.vhdl
elab_simulate tb_delayline1d

clean

analyze delayline.vhdl tb_delayline.vhdl
elab_simulate tb_delayline

clean

echo "Test successful"
