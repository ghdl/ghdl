#! /bin/sh

. ../../testenv.sh

analyze --std=02 file1.vhd
analyze --std=02 file2.vhd

clean

echo "Test successful"
