#! /bin/sh

. ../../testenv.sh

$GHDL -i hello.vhdl pkg.vhdl
sleep 1
$GHDL -r --expect-failure hello

clean

echo "Test successful"
