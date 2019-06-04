#! /bin/sh

. ../../testenv.sh

if analyze --std=08 "" r.vhdl; then
  echo "error expected"
  exit 1;
fi

clean

echo "Test successful"
