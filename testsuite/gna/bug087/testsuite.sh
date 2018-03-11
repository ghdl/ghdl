#! /bin/sh

. ../../testenv.sh

if $GHDL -a; then
  echo "Error message expected"
  exit 1;
fi


echo "Test successful"
