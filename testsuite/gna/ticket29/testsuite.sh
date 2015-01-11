#! /bin/sh

. ../../testenv.sh

analyze debugtools.vhdl
analyze iomapper.vhdl

clean

echo "Test successful"
