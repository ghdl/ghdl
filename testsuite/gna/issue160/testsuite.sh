#! /bin/sh

. ../../testenv.sh

GHDL_STD_FLAGS=--std=87
analyze top.vhdl
elab_failure top
clean

GHDL_STD_FLAGS=--std=93
analyze top.vhdl
elab_failure top
clean

GHDL_STD_FLAGS=--std=02
analyze top.vhdl
elab_failure top
clean

GHDL_STD_FLAGS=--std=08
analyze top.vhdl
elab_failure top
unset GHDL_STD_FLAGS
elab_failure top
GHDL_STD_FLAGS=--std=08
clean

echo "Test successful"
