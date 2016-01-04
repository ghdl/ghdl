#! /bin/sh

. ../../testenv.sh

$GHDL -s --ams ams1.vhdl

clean

echo "Test successful"
