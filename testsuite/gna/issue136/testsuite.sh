#! /bin/sh

. ../../testenv.sh

if $GHDL @unknown.rsp 2> nexist.err; then
  echo "failure expected"
  exit 1
fi
if grep -q Bug nexist.err; then
  echo "Bug box not expected"
  exit 1
fi
rm -f nexist.err

$GHDL @args.resp

rm -rf html

clean

echo "Test successful"
