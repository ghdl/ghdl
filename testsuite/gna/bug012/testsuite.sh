#! /bin/sh

. ../../testenv.sh

analyze demo.vhd
elab_simulate bar structural

clean

echo "Test successful"
