#! /bin/sh

. ../../testenv.sh

analyze --std=02 -Wdelayed-checks example.vhdl

clean

echo "Test successful"
