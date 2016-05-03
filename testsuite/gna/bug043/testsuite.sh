#! /bin/sh

. ../../testenv.sh

analyze --ieee=synopsys -fexplicit sha256.vhd

clean

echo "Test successful"
