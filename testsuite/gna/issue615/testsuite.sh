#! /bin/sh

. ../../testenv.sh

export GHDL_STD_FLAGS=--std=08
analyze repr.vhdl
elab repr
if ghdl_has_feature repr ghw; then
  simulate repr --wave=repr.ghw
  ghw_diff repr
  rm -f repr.txt repr.ghw
fi

clean

echo "Test successful"
