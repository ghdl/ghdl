#! /bin/sh

. ../../testenv.sh

if $GHDL --version | grep -q mcode; then
  $GHDL -i hello.vhdl pkg.vhdl
  sleep 1
  $GHDL -r --expect-failure hello
fi

clean

echo "Test successful"
