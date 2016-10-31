#! /bin/sh

. ../../testenv.sh

$GHDL --file-to-xml t2.vhdl | grep -q "01X"
$GHDL --file-to-xml test.vhdl | grep -q '"00"'
clean

echo "Test successful"
