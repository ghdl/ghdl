#! /bin/sh

. ../../testenv.sh

$GHDL --file-to-xml ex.vhdl > ex.xml

rm -f ex.xml

echo "Test successful"
