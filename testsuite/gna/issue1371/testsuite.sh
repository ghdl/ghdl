#! /bin/sh

. ../../testenv.sh

export GHDL_STD_FLAGS=-fpsl
analyze incr_psl.vhdl

clean

echo "Test successful"
