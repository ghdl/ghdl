#! /bin/sh

. ../../testenv.sh

$GHDL @args.resp

rm -rf html

clean

echo "Test successful"
