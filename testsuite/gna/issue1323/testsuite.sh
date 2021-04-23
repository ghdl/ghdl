#! /bin/sh

. ../../testenv.sh

analyze mydesign.vhdl
elab myentity
if ghdl_has_feature myentity ghw; then
  elab_simulate myentity --wave=dump.ghw | tee mydesign.out
  ghw_diff dump
  rm -f mydesign.out dump.txt dump.ghw
fi

clean

echo "Test successful"
