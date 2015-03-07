#! /bin/sh

. ../../testenv.sh

analyze dispgen.vhdl
elab_simulate dispgen

elab_simulate dispgen -gstr=Hello

clean

echo "Test successful"
