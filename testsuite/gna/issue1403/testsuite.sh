#! /bin/sh

. ../../testenv.sh


analyze ent.vhdl
$GHDL --gen-makefile -frelaxed ent > Makefile
grep -q relaxed Makefile

rm -f Makefile
clean

echo "Test successful"
