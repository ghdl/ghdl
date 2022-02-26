#! /bin/sh

. ../../testenv.sh

export GHDL_STD_FLAGS=--std=08

analyze --work=osvvm NamePkg.vhd
analyze --work=osvvm OsvvmGlobalPkg.vhd
analyze --work=osvvm TextUtilPkg.vhd
analyze --work=osvvm AlertLogPkg.vhd
analyze repro.vhdl

elab_simulate repro

clean osvvm
clean

echo "Test successful"
