#! /bin/sh

. ../../testenv.sh

export GHDL_STD_FLAGS=--std=08
if $GHDL --version | grep -q mcode; then
  analyze --check-ast assert01.vhdl
fi

clean

echo "Test successful"
