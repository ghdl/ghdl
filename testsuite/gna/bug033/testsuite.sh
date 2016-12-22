#! /bin/sh

. ../../testenv.sh

analyze simple.vhdl
elab_simulate simple

$GHDL --reprint simple.vhdl
$GHDL --pp-html simple.vhdl > /dev/null
$GHDL --xref-html simple.vhdl
$GHDL --xref-html --std=08 simple08.vhdl

rm -rf html

clean

echo "Test successful"
