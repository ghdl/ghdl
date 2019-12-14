#! /bin/sh

. ../../testenv.sh

$GHDL --disp-config --PREFIX=myprefix | grep "command line" | grep myprefix
GHDL_PREFIX=myenvprefix $GHDL --disp-config | grep GHDL_PREFIX | grep envprefix

echo "Test successful"
