#! /bin/sh

. ../../testenv.sh

if ! $GHDL --help -a | grep -q time-resolution; then
  echo "option --time-resolution not available"
else
  export GHDL_STD_FLAGS=--std=08
  analyze a.vhdl
  elab_simulate --time-resolution=ns a | tee a.out
  diff_nocr a.out a.ref
  clean
fi

echo "Test successful"
