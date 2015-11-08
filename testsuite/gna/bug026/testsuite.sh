#! /bin/sh

. ../../testenv.sh

analyze x.vhd
analyze xb.vhd

clean

echo "Test successful"
