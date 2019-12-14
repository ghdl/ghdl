#! /bin/sh

. ../../testenv.sh

GHDL_STD_FLAGS=--workdir=work1
rm -rf work1
mkdir work1
analyze simple.vhdl
elab_simulate simple

clean

rmdir work1

# Some simple checks
if test -f simple || test -f 'e~simple.o'; then
    echo "Not correctly cleaned"
    ls -l
    exit 1
fi

echo "Test successful"
