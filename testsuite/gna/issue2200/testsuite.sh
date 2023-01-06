#! /bin/sh

. ../../testenv.sh

$GHDL -i --ieee=synopsys --warn-unused a.vhdl
$GHDL -m --ieee=synopsys --warn-unused -Werror a

clean

echo "Test successful"
