#! /bin/sh

. ../../testenv.sh

if $GHDL -i t.vhdl; then
  echo "Failure expected"
  exit 1
fi

clean

echo "Test successful"
