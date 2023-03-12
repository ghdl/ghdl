#! /bin/sh

. ../../testenv.sh

run $GHDL -i top_a.vhd top_e.vhd
run $GHDL -m unit_a
simulate unit_a

clean

echo "Test successful"
