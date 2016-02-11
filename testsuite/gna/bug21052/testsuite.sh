#! /bin/sh

. ../../testenv.sh

analyze test.vhd
if ghdl_has_feature test ghw; then
  elab_simulate test --wave=test.ghw
fi

clean

echo "Test successful"
