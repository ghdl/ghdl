#! /bin/sh

. ../../testenv.sh

export GHDL_STD_FLAGS=--std=08

if $GHDL --version | grep -q "GCC .* code generator"; then
  # Only -ftest-coverage: no need for libgcov to link.
  GHDL_FLAGS="-ftest-coverage"
fi

analyze repro.vhdl
elab_simulate repro

rm -f *.gcno
clean

echo "Test successful"
