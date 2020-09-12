#! /bin/sh

. ../../testenv.sh

export GHDL_STD_FLAGS="--std=02 --work=mwe -frelaxed"
analyze mwe.vhdl

clean

export GHDL_STD_FLAGS="--std=02 --work=mwe"
analyze_failure mwe.vhdl

clean

echo "Test successful"
