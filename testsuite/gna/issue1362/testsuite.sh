#! /bin/sh

. ../../testenv.sh

$GHDL -c --work=liba liba.vhdl --work=libb libb.vhdl --work=work ent.vhdl -e ent

clean

echo "Test successful"
