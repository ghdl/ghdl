#! /bin/sh

. ../../testenv.sh

if $GHDL -c ent.vhdl -e ent; then
  echo "Error expected"
  exit 1
fi

echo "Test successful"
