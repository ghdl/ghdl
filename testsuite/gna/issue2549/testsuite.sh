#! /bin/sh

. ../../testenv.sh

export GHDL_STD_FLAGS=--std=08
analyze pkg_types.vhd
analyze pkg_foo_public.vhd
analyze foo.vhd
analyze tb_foo.vhd
elab_simulate tb_foo --stop-time=1us

clean

echo "Test successful"
