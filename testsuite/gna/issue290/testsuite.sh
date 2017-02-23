#! /bin/sh

. ../../testenv.sh

export GHDL_STD_FLAGS=--std=08
analyze TbNamesPkg.vhd TbNames.vhd
elab_simulate TbNames

clean

echo "Test successful"
