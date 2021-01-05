#! /bin/sh

. ../../testenv.sh

run "$GHDL" --reprint --std=08 assert3.vhdl > /dev/null

echo "Test successful"
