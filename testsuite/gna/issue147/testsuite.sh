#! /bin/sh

echo "Test skipped"
exit 0

. ../../testenv.sh

GHDL_STD_FLAGS=--workdir=work1
mkdir work1
analyze simple.vhdl
elab_simulate simple

clean

rmdir work1

# Some simple checks
if test -f simple || test -f 'e~simple.o'; then
  echo "Not correctly cleaned"
  exit 1
fi

echo "Test successful"
