#! /bin/sh

. ../../testenv.sh

analyze --ieee=synopsys repro.vhdl

clean

echo "Test successful"
