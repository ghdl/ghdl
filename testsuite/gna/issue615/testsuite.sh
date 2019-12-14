#! /bin/sh

. ../../testenv.sh

export GHDL_STD_FLAGS=--std=08
analyze repr.vhdl
elab repr
if ghdl_has_feature repr ghw; then
  simulate repr --wave=repr.ghw
# TODO: check with ghwdump ?
fi
 

clean

echo "Test successful"
