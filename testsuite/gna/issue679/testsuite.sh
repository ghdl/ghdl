#! /bin/sh

. ../../testenv.sh

analyze -fpsl repro.vhdl

clean

echo "Test successful"
