#! /bin/sh

. ../../testenv.sh

$GHDL -i ent.vhdl
$GHDL -m --expect-failure ent

clean

echo "Test successful"
