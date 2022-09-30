#! /bin/sh

. ../../testenv.sh

analyze e.vhdl
$GHDL --file-to-xml > e.xml

clean

echo "Test successful"
