#! /bin/sh

. ../../testenv.sh

$GHDL -i tb.vhdl a.vhdl
if $GHDL -c -e tb; then
  echo "failure expected"
  exit 1;
fi

clean

echo "Test successful"
