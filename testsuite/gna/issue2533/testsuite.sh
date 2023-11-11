#! /bin/sh

. ../../testenv.sh

$GHDL import --work=zork foo.vhdl foobar.vhdl
$GHDL import foobar_tb.vhdl

$GHDL -a -Werror=library --work=zork --workdir=. foo.vhdl

$GHDL make -v -Werror=library foobar_tb

clean

echo "Test successful"
